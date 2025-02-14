if ! command -v shadercross 2>&1 >/dev/null
then
    echo "❌ shadercross could not be found, please make sure https://github.com/libsdl-org/SDL_shadercross is installed"
    exit 1
fi

echo "⏳ Transpiling vertex shaders ..."
for filename in *.vert.hlsl; do
    if [ -f "$filename" ]; then
        shadercross "$filename" -o "compiled_shaders/${filename/.hlsl/.spv}"
        shadercross "$filename" -o "compiled_shaders/${filename/.hlsl/.msl}"
        # perl -pi.bak -e 's/main0/main/g' "compiled_shaders/${filename/.hlsl/.msl}"
        shadercross "$filename" -o "compiled_shaders/${filename/.hlsl/.dxil}"
    fi
done
echo "✅ Vertex shaders transpiled"

echo "⏳ Transpiling fragment shaders ..."
for filename in *.frag.hlsl; do
    if [ -f "$filename" ]; then
        shadercross "$filename" -o "compiled_shaders/${filename/.hlsl/.spv}"
        shadercross "$filename" -o "compiled_shaders/${filename/.hlsl/.msl}"
        # perl -pi.bak -e 's/main0/main/g' "compiled_shaders/${filename/.hlsl/.msl}"
        shadercross "$filename" -o "compiled_shaders/${filename/.hlsl/.dxil}"
    fi
done
echo "✅ Fragment shaders transpiled"