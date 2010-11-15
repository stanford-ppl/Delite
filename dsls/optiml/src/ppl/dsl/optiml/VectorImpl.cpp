/**
 * This is the actual class that gets instantiated in the generated code. Ops corresponding to public operations
 * here must have CodeGen methods defined by the DSL on them.
 *
 * Alternatively, everything in this class could be lifted, and we could generate a concrete class to be instantiated
 * in the generated code.
 */

template <class T>
class VectorImpl {

    private:
        T *_data;

    public:

        // public fields
        int length;
        bool is_row;

        // Constructor
        VectorImpl(int _len, bool _isRow) {
            length = _len;
            isRow = _isRow;
            _data = new malloc(sizeof(T)*_length);
        }

        // accessor functions
        T apply(int idx) { return _data(idx); }
        void update(int idx, T newVal) { _data(idx) = newVal; }

        //int length() { return _length; }
        //bool is_row() { return _is_row; }

        // Not implemented yet 
        /*
        def +=[A <: T](x: A): VectorImpl[T] = {
            ensureExtra(1)
            _data(_length) = x
            _length += 1
            this
        }

        protected def ensureExtra(extra: Int) {
            if (_data.length - _length < extra) {
                realloc(_length + extra)
            }
        }

        protected def realloc(minLen: Int) {
            var n = 4 max (_data.length * 2)
            while (n < minLen) n *= 2
            val d = new Array[T](n)
            Array.copy(_data, 0, d, 0, _length)
            _data = d
        }
        */

}
