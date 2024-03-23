signature TEXTURE_TYPE =
sig
  datatype t = None | Alpha | Rgba
end

structure TextureType = struct datatype t = None | Alpha | Rgba end
