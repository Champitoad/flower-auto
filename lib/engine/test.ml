module A = struct
  module B = struct
   let x = 2
  end
end

let _ = A.B.x