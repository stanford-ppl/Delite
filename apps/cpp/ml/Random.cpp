#ifndef RANDOM_DEFINED
#define RANDOM_DEFINED

#include <math.h>

class Random {
    
    private: 
    long long seed;
    
    const static long long multiplier = 0x5DEECE66DLL;
    const static long long addend = 0xBLL;
    const static long long mask = (1LL << 48) - 1;
	
    public: 
    Random(long long seed) {
        this->seed = initialScramble(seed);
    	haveNextNextGaussian = false;
    }

    private: 
    static long long initialScramble(long long seed) {
        return (seed ^ multiplier) & mask;
    }
	
    public: 
    void setSeed(long long seed) {
        this->seed = initialScramble(seed);
        haveNextNextGaussian = false;
    }
	
    protected: 
    int next(int bits) {
        long long nextseed;
        long long oldseed = this->seed;
        nextseed = (oldseed * multiplier + addend) & mask;
        this->seed = nextseed;
        return (int)(((unsigned long long) nextseed) >> (48 - bits));
    }
	
    public: 
    int nextInt() {
        return next(32);
    }
	
    public: 
    long long nextLong() {
        // it's okay that the bottom word remains signed.
        return ((long long)(next(32)) << 32) + next(32);
    }
	
    public: 
    bool nextBoolean() {
        return next(1) != 0;
    }
	
    public: 
    float nextFloat() {
        return next(24) / ((float)(1 << 24));
    }
	
    public: 
    double nextDouble() {
        return (((long long)(next(26)) << 27) + next(27)) / (double)(1LL << 53);
    }
	
    private: 
    double nextNextGaussian;
    bool haveNextNextGaussian;
	
    public: 
    double nextGaussian() {
	// See Knuth, ACP, Section 3.4.1 Algorithm C.
        if (haveNextNextGaussian) {
            haveNextNextGaussian = false;
            return nextNextGaussian;
        } else {
            double v1, v2, s;
            do {
                v1 = 2 * nextDouble() - 1; // between -1 and 1
                v2 = 2 * nextDouble() - 1; // between -1 and 1
                s = v1 * v1 + v2 * v2;
            } while (s >= 1 || s == 0);
            double multiplier = sqrt(-2 * log(s)/s);
            nextNextGaussian = v2 * multiplier;
            haveNextNextGaussian = true;
            return v1 * multiplier;
        }
    }

};

#endif

