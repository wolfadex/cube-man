package main

import "base:runtime"
import "core:log"
import "core:mem"
import "core:fmt"
import "core:os"

import sdl "vendor:sdl3"

when ODIN_OS == .Darwin {
    shader_format := sdl.GPUShaderFormat{.MSL}
    frag_shader_code := #load("compiled_shaders/shader.frag.msl")
    vert_shader_code := #load("compiled_shaders/shader.vert.msl")
} else when ODIN_OS == .Windows {
    shader_format := sdl.GPUShaderFormat{.DXIL}
    frag_shader_code := #load("compiled_shaders/shader.frag.dxil")
    vert_shader_code := #load("compiled_shaders/shader.vert.dxil")
} else {
    shader_format := sdl.GPUShaderFormat{.SPIRV}
    frag_shader_code := #load("compiled_shaders/shader.frag.spv")
    vert_shader_code := #load("compiled_shaders/shader.vert.spv")
}


main :: proc() {
    when ODIN_DEBUG {
		// setup debug logging
		logger := log.create_console_logger()
		context.logger = logger

		// setup tracking allocator for making sure all memory is cleaned up
		default_allocator := context.allocator
		tracking_allocator: mem.Tracking_Allocator
		mem.tracking_allocator_init(&tracking_allocator, default_allocator)
		context.allocator = mem.tracking_allocator(&tracking_allocator)

		reset_tracking_allocator :: proc(a: ^mem.Tracking_Allocator) -> bool {
			err := false

			for _, value in a.allocation_map {
				fmt.printfln("%v: Leaked %v bytes", value.location, value.size)
				err = true
			}

			mem.tracking_allocator_clear(a)

			return err
		}

		sdl.SetLogPriorities(.VERBOSE)
		sdl.SetLogOutputFunction(proc "c" (userdata: rawptr, category: sdl.LogCategory, priority: sdl.LogPriority, message: cstring) {
		    context = runtime.default_context()
			log.debug("SDL {} [{}]: {}", category, priority, message)
		}, nil)
	}

    log.debug("hello")

    ok := sdl.Init({.VIDEO})
    assert(ok)

    window := sdl.CreateWindow("Cube-Man", 800, 600, {})
    assert(window != nil)

    when ODIN_DEBUG {
        gpu_debug := true
    } else {
        gpu_debug := false
    }

    gpu := sdl.CreateGPUDevice(shader_format, gpu_debug, nil)
    assert(gpu != nil)

    ok = sdl.ClaimWindowForGPUDevice(gpu, window)
    assert(ok)

    vert_shader := sdl.CreateGPUShader(gpu, {
        code_size  = len(vert_shader_code),
        code       = raw_data(vert_shader_code),
        entrypoint = "main0",
        format     = shader_format,
        stage      = .VERTEX
    })
    if (vert_shader == nil) {
        log.debug("SDL", sdl.GetError())
        assert(vert_shader != nil)
    }
    frag_shader := sdl.CreateGPUShader(gpu, {
        code_size  = len(frag_shader_code),
        code       = raw_data(frag_shader_code),
        entrypoint = "main0",
        format     = shader_format,
        stage      = .FRAGMENT
    })
    assert(frag_shader != nil)

    pipeline := sdl.CreateGPUGraphicsPipeline(gpu, {
        vertex_shader   = vert_shader,
        fragment_shader = frag_shader,
        primitive_type  = .TRIANGLELIST,
        target_info     = {
            num_color_targets = 1,
            color_target_descriptions = &(sdl.GPUColorTargetDescription {
                format = sdl.GetGPUSwapchainTextureFormat(gpu, window),
            }),
        },
    })

    sdl.ReleaseGPUShader(gpu, vert_shader)
    sdl.ReleaseGPUShader(gpu, frag_shader)

    main_loop: for {
        event : sdl.Event

        for sdl.PollEvent(&event) {
            #partial switch event.type {
            case .QUIT:
                break main_loop
            case .KEY_DOWN:
                if event.key.scancode == .ESCAPE {
                    break main_loop
                }
            }
        }

        // update

        // render
        command_buffer := sdl.AcquireGPUCommandBuffer(gpu)

        swap_chain_texture: ^sdl.GPUTexture
        ok = sdl.WaitAndAcquireGPUSwapchainTexture(command_buffer, window, &swap_chain_texture, nil, nil)
        assert(ok)

        color_target := sdl.GPUColorTargetInfo {
            texture     = swap_chain_texture,
            load_op     = .CLEAR,
            clear_color = {0.3921568627, 0.5843137255, 0.9294117647, 1.0},
            store_op    = .STORE
        }
        render_pass := sdl.BeginGPURenderPass(command_buffer, &color_target, 1, nil)

        sdl.BindGPUGraphicsPipeline(render_pass, pipeline)

        sdl.DrawGPUPrimitives(render_pass, 3, 1, 0, 0)

        sdl.EndGPURenderPass(render_pass)

        ok = sdl.SubmitGPUCommandBuffer(command_buffer)
        assert(ok)
    }


}