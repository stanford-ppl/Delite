#include <unordered_map>
#include <iostream>
#include <jni.h>
#include <time.h>
#include <omp.h>
#include <sys/time.h>

using namespace std;

int numThreads = 1;

struct ArrayQ1 {
	int length;
	double* l_quantity;
	double* l_extendedprice;
	double* l_discount;
	double* l_tax;
  jchar* l_returnflag;
	jchar* l_linestatus;
	int* l_shipdate;
};

struct ArrayResult {
	int length;
	jchar* returnFlag;
  jchar* lineStatus;
	double* sumQty;
	double* sumBasePrice;
	double* sumDiscountedPrice;
	double* sumCharge;
	double* avgQty;
	double* avgPrice;
	double* avgDiscount;
	int* countOrder;
};

JNIEnv* env;

double discounted(double extended, double discount) {
	return extended * (1.0 - discount);
}

double charge(double extended, double discount, double tax) {
	return extended * (1.0 - discount) * (1.0 + tax);
 }

void resize(ArrayResult* out) {
	cout << "ERROR: need to resize" << endl;
	exit(-1);
}

void trim(ArrayResult* out, int length) {
	out->length = length;
}

ArrayResult query1(ArrayQ1 in, int date, ArrayResult out) {
 	typedef pair <int, int> KV;
  //int numThreads = omp_get_num_threads();
  //printf("openmp get num threads: %d\n", numThreads);
  
  // thread-local buffers with fixed max size      
	//unordered_map<int, int>* buf_maps = (unordered_map<int,int>*) malloc(numThreads*sizeof(unordered_map<int,int>)); 
  unordered_map<int,int> buf_maps[numThreads];
  //ArrayResult* bufs = (ArrayResult*) malloc(numThreads*sizeof(ArrayResult));
  ArrayResult bufs[numThreads];

  for (int i = 0; i < numThreads; i++) {
    //bufs[i] = malloc(sizeof(ArrayResult));
    int length = out.length;

    bufs[i].length = length;
    bufs[i].returnFlag = (jchar*) malloc(sizeof(jchar)*length); 
    bufs[i].lineStatus = (jchar*) malloc(sizeof(jchar)*length);
    bufs[i].sumQty = (double*) malloc(sizeof(double)*length);
    bufs[i].sumBasePrice = (double*) malloc(sizeof(double)*length);
    bufs[i].sumDiscountedPrice = (double*) malloc(sizeof(double)*length);
    bufs[i].sumCharge = (double*) malloc(sizeof(double)*length);
    bufs[i].avgQty = (double*) malloc(sizeof(double)*length);
    bufs[i].avgPrice = (double*) malloc(sizeof(double)*length);
    bufs[i].avgDiscount = (double*) malloc(sizeof(double)*length);
    bufs[i].countOrder = (int*) malloc(sizeof(int)*length);
  }  

  #pragma omp parallel
  { 
  int currentIndex = 0;
	int key; 
	unordered_map<int, int> :: const_iterator mapValue;
	pair < unordered_map<int, int> :: const_iterator, bool > success;

  int t = omp_get_thread_num();
  #pragma omp for schedule(static)
	for (int i = 0; i < in.length; i++) { //groupBy
    //printf("tid: %d, input elem: %d\n", t, i);    

		if (in.l_shipdate[i] < date) {
			key = in.l_returnflag[i] << 16 + in.l_linestatus[i];
			success = buf_maps[t].insert(KV(key, currentIndex));
			int idx;
			if (success.second) { //new key
				idx = currentIndex;
				currentIndex++;
			}
			else { //existing key
				mapValue = buf_maps[t].find(key);
				idx = mapValue -> second;
			}      

			if (idx >= bufs[t].length) {
				cout << "currentIdx = " << idx << endl;
        cout << "bufs[t].length = " << bufs[t].length << endl;
        resize(&bufs[t]);
			}
      
			if (success.second) { //insert first elem
        //printf("inserting into bufs[%d]\n", t);
				bufs[t].returnFlag[idx] = in.l_returnflag[i];
      	bufs[t].lineStatus[idx] = in.l_linestatus[i];
      	bufs[t].sumQty[idx] = in.l_quantity[i];
      	bufs[t].sumBasePrice[idx] = in.l_extendedprice[i];
      	bufs[t].sumDiscountedPrice[idx] = discounted(in.l_extendedprice[i], in.l_discount[i]);
      	bufs[t].sumCharge[idx] = charge(in.l_extendedprice[i], in.l_discount[i], in.l_tax[i]);
      	bufs[t].avgQty[idx] = in.l_quantity[i];
     		bufs[t].avgPrice[idx] = in.l_extendedprice[i];
      	bufs[t].avgDiscount[idx] = in.l_discount[i];
      	bufs[t].countOrder[idx] = 1;
			}
			else { //reduce with existing elem
        //printf("reducing into bufs[%d]\n", t);
      	bufs[t].sumQty[idx] += in.l_quantity[i];
      	bufs[t].sumBasePrice[idx] += in.l_extendedprice[i];
      	bufs[t].sumDiscountedPrice[idx] += discounted(in.l_extendedprice[i], in.l_discount[i]);
      	bufs[t].sumCharge[idx] += charge(in.l_extendedprice[i], in.l_discount[i], in.l_tax[i]);
      	bufs[t].avgQty[idx] += in.l_quantity[i];
     		bufs[t].avgPrice[idx] += in.l_extendedprice[i];
      	bufs[t].avgDiscount[idx] += in.l_discount[i];
      	bufs[t].countOrder[idx] += 1;
		  }
		}
	}

  trim(&bufs[t], currentIndex);

  }

  // reduce thread-local buffers
  //printf("reduce thread-local buffers\n");

  // need copy for first buf?
  //assert(bufs[0].length > 0);
               
  //out.returnFlag = bufs[0].returnFlag;
  //out.lineStatus = bufs[0].lineStatus;
  //out.sumQty = bufs[0].sumQty;
  //out.sumBasePrice = bufs[0].sumBasePrice;
  //out.sumDiscountedPrice = bufs[0].sumDiscountedPrice;
  //out.sumCharge = bufs[0].sumCharge;
  //out.avgQty = bufs[0].avgQty;
  //out.avgDiscount = bufs[0].avgDiscount;
  //out.countOrder = bufs[0].countOrder;
      
	unordered_map<int, int> out_map;
	unordered_map<int, int> :: const_iterator mapValue;
	unordered_map<int, int> :: const_iterator buf_mapValue;
  pair < unordered_map<int, int> :: const_iterator, bool > success;
  int currentIndex = 0;

  for (int i = 0; i < numThreads; i++) {
    for (int j = 0; j < bufs[i].length; j++) {
      int key = bufs[i].returnFlag[j] << 16 + bufs[i].lineStatus[j];
      buf_mapValue = buf_maps[i].find(key);        
      int buf_idx = buf_mapValue -> second;
			
      success = out_map.insert(KV(key, currentIndex)); 
      int idx;
			if (success.second) { //new key
				idx = currentIndex;
				currentIndex++;
			}
			else { //existing key
				mapValue = out_map.find(key);
				idx = mapValue -> second;
			}

			if (idx >= out.length) {
				cout << "currentIdx = " << idx << endl;
        cout << "out.length = " << out.length << endl;
        resize(&out);
			}

			if (success.second) { //insert first elem
				out.returnFlag[idx] = bufs[i].returnFlag[buf_idx];
      	out.lineStatus[idx] = bufs[i].lineStatus[buf_idx];
      	out.sumQty[idx] = bufs[i].sumQty[buf_idx];
      	out.sumBasePrice[idx] = bufs[i].sumBasePrice[buf_idx];
      	out.sumDiscountedPrice[idx] = bufs[i].sumDiscountedPrice[buf_idx];
      	out.sumCharge[idx] = bufs[i].sumCharge[buf_idx];
      	out.avgQty[idx] = bufs[i].avgQty[buf_idx];
     		out.avgPrice[idx] = bufs[i].avgPrice[buf_idx];
      	out.avgDiscount[idx] = bufs[i].avgDiscount[buf_idx];
      	out.countOrder[idx] = bufs[i].countOrder[buf_idx];
			}
			else { //reduce with existing elem
      	out.sumQty[idx] += bufs[i].sumQty[buf_idx];
      	out.sumBasePrice[idx] += bufs[i].sumBasePrice[buf_idx];
      	out.sumDiscountedPrice[idx] += bufs[i].sumDiscountedPrice[buf_idx];
      	out.sumCharge[idx] += bufs[i].sumCharge[buf_idx];
      	out.avgQty[idx] += bufs[i].avgQty[buf_idx];
     		out.avgPrice[idx] += bufs[i].avgPrice[buf_idx];
      	out.avgDiscount[idx] += bufs[i].avgDiscount[buf_idx];
      	out.countOrder[idx] += bufs[i].countOrder[buf_idx];
		  }
		} 
  }

  trim(&out, currentIndex);

  //#pragma omp parallel for
	for (int i = 0; i < out.length; i++) { //average
		int count = out.countOrder[i];
		out.avgQty[i] /= count;
		out.avgPrice[i] /= count;
		out.avgDiscount[i] /= count;
	}

	//sort: returnFlag then lineStatus
	return out;
}

/* double* fill(int length) {
	double* a = new double[length];
	for (int i = 0; i < length; i++) {
		a[i] = (double) i;
	}
	return a;
}

short* fillFlag(int length) {
	short* a = new short[length];
	for (int i = 0; i < length; i++) {
		if (i < length/2) {
			a[i] = 1;
		}
		else {
			a[i] = 2;
		}
	}
	return a;
}

int* fillInt(int length) {
	int* a = new int[length];
	for (int i = 0; i < length; i++) {
		a[i] = 0;
	}
	return a;
}

int main(int argc, const char* argv[]) {
	int length = 5;
	ArrayQ1 in;
	in.length = length;
	in.l_quantity = fill(length);
	in.l_extendedprice = fill(length);
	in.l_discount = fill(length);
	in.l_tax = fill(length);
	in.l_returnflag = fillFlag(length);
	in.l_linestatus = fillFlag(length);
	in.l_shipdate = fillInt(length);

	ArrayResult out = query1(in, 100);
	for (int i = 0; i < out.length; i++) {
		cout << out.returnFlag[i] << endl;
	}

	return 0;
} */

void release(jarray object, void* ptr) {
	env->ReleasePrimitiveArrayCritical(object, ptr, 0);
}

void* get(jarray array) {
	return env->GetPrimitiveArrayCritical(array, 0);
}

extern "C" JNIEXPORT jobject JNICALL Java_Query1_00024_query1(JNIEnv* jnienv, jobject thisObj, jobject object, jint date);
JNIEXPORT jobject JNICALL Java_Query1_00024_query1(JNIEnv* jnienv, jobject thisObj, jobject object, jint date) {
	env = jnienv;
	jclass cls = jnienv->FindClass("ArrayQ1");
	//cout << cls << endl;
    ArrayQ1 in;
	//cout << jnienv->GetMethodID(cls, "length", "()I") << endl;
    //cout << jnienv->GetMethodID(cls, "l_quantity", "()[D") << endl;
    //cout << (bool) jnienv->IsInstanceOf(object, cls) << endl;

    in.length = jnienv->CallIntMethod(object, jnienv->GetMethodID(cls, "length", "()I"));
	//cout << "after length" << endl;
    jarray q = (jarray) jnienv->CallObjectMethod(object, jnienv->GetMethodID(cls, "l_quantity", "()[D"));
	jarray e = (jarray) jnienv->CallObjectMethod(object, jnienv->GetMethodID(cls, "l_extendedprice", "()[D"));
	jarray d = (jarray) jnienv->CallObjectMethod(object, jnienv->GetMethodID(cls, "l_discount", "()[D"));
	jarray t = (jarray) jnienv->CallObjectMethod(object, jnienv->GetMethodID(cls, "l_tax", "()[D"));
	jarray r = (jarray) jnienv->CallObjectMethod(object, jnienv->GetMethodID(cls, "l_returnflag", "()[C"));
	jarray l = (jarray) jnienv->CallObjectMethod(object, jnienv->GetMethodID(cls, "l_linestatus", "()[C"));
	jarray s = (jarray) jnienv->CallObjectMethod(object, jnienv->GetMethodID(cls, "l_shipdate", "()[I"));
    //cout << "after arrays" << endl;

	in.l_quantity = (double*) get(q);
	in.l_extendedprice = (double*) get(e);
	in.l_discount = (double*) get(d);
	in.l_tax = (double*) get(t);
	in.l_returnflag = (jchar*) get(r);
	in.l_linestatus = (jchar*) get(l);
	in.l_shipdate = (int*) get(s);

    //timeStart
    clock_t start = clock();
    //cout << start << endl;

    struct timeval myprofiler_start, myprofiler_end;
    gettimeofday(&myprofiler_start,NULL);

	int length = 128;
	jarray rf = jnienv->NewCharArray(length);
	jarray ls = jnienv->NewCharArray(length);
	jarray sq = jnienv->NewDoubleArray(length);
	jarray sb = jnienv->NewDoubleArray(length);
	jarray sd = jnienv->NewDoubleArray(length);
	jarray sc = jnienv->NewDoubleArray(length);
	jarray aq = jnienv->NewDoubleArray(length);
	jarray ap = jnienv->NewDoubleArray(length);
	jarray ad = jnienv->NewDoubleArray(length);
	jarray co = jnienv->NewIntArray(length);

	ArrayResult out;
	out.length = length;
	out.returnFlag = (jchar*) get(rf);
	out.lineStatus = (jchar*) get(ls);
	out.sumQty = (double*) get(sq);
	out.sumBasePrice = (double*) get(sb);
	out.sumDiscountedPrice = (double*) get(sd);
	out.sumCharge = (double*) get(sc);
	out.avgQty = (double*) get(aq);
	out.avgPrice = (double*) get(ap);
	out.avgDiscount = (double*) get(ad);
	out.countOrder = (int*) get(co);

    //timeCheck
    //clock_t call = clock();
	//cout << call << endl;
    ArrayResult res = query1(in, date, out);
    //timeStop
    gettimeofday(&myprofiler_end,NULL);
    printf("Total Time : %ld [us]\n", ((myprofiler_end.tv_sec * 1000000 + myprofiler_end.tv_usec) - (myprofiler_start.tv_sec * 1000000 + myprofiler_start.tv_usec)));
    //clock_t end = clock();
    //cout << end << endl;
    //cout << "Malloc Time: " << (call-start)*1.0/CLOCKS_PER_SEC << endl;
    //cout << "Time: " << (end-start)*1.0/CLOCKS_PER_SEC << endl;


	release(q, in.l_quantity); release(e, in.l_extendedprice); release(d, in.l_discount); release(t, in.l_tax); release(r, in.l_returnflag); release(l, in.l_linestatus); release(s, in.l_shipdate);
	release(rf, out.returnFlag); release(ls, out.lineStatus); release(sq, out.sumQty); release(sb, out.sumBasePrice); release(sd, out.sumDiscountedPrice); 
    release(sc, out.sumCharge); release(aq, out.avgQty); release(ap, out.avgPrice); release(ad, out.avgDiscount); release(co, out.countOrder);
    //cout << "after release" << endl;

	jclass cls1 = jnienv->FindClass("ArrayResult$");
	//cout << cls1 << endl;
    //cout << jnienv->GetStaticFieldID(cls1, "MODULE$", "LArrayResult$;") << endl;
    jobject module = jnienv->GetStaticObjectField(cls1,jnienv->GetStaticFieldID(cls1,"MODULE$","LArrayResult$;"));
    //cout << module << endl;
                                                                    
    //cout << jnienv->GetMethodID(cls1, "apply", "(I[C[C[D[D[D[D[D[D[D[I)LArrayResult;") << endl;                                                                  
    jobject scalaOut = jnienv->CallObjectMethod(module, jnienv->GetMethodID(cls1, "apply", "(I[C[C[D[D[D[D[D[D[D[I)LArrayResult;"), res.length, rf, ls, sq, sb, sd, sc, aq, ap, ad, co);
	return scalaOut;
}

int main() {
    bool good = sizeof(short) == sizeof(jchar);
    cout << good << endl;
}

