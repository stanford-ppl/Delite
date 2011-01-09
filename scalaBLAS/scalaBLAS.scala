//package ppl.scalaBLAS

object scalaBLAS {

    System.load("/home/hyouklee/delite/Delite/scalaBLAS/scalaBLAS.so")

    @native
    def matMult[@specialized(Double,Float) T](mat1:Array[T], mat2:Array[T], mat3:Array[T], mat1_r:Int, mat1_c:Int, mat2_c:Int)
    
    @native
    def matVMult[@specialized(Double,Float) T](mat1:Array[T], vec2:Array[T], vec3:Array[T], mat_row:Int, mat_col:Int, vec_offset:Int, vec_stride:Int)
}
