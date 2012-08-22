#ifndef _DELITEARRAY_CL_
#define _DELITEARRAY_CL_

#define CHAR short
#define jCHAR jshort

typedef struct {
	int length;
	__global bool *data;
} DeliteArray_bool;

int DeliteArray_bool_size(DeliteArray_bool da) {
	return da.length;
}
bool DeliteArray_bool_apply(DeliteArray_bool da, int idx) {
	return da.data[idx];
}
void DeliteArray_bool_update(DeliteArray_bool da, int idx, bool value) {
	da.data[idx] = value;
}

typedef struct {
	int length;
	__global char *data;
} DeliteArray_char;

int DeliteArray_char_size(DeliteArray_char da) {
	return da.length;
}
char DeliteArray_char_apply(DeliteArray_char da, int idx) {
	return da.data[idx];
}
void DeliteArray_char_update(DeliteArray_char da, int idx, char value) {
	da.data[idx] = value;
}

typedef struct {
	int length;
	__global CHAR *data;
} DeliteArray_CHAR;

int DeliteArray_CHAR_size(DeliteArray_CHAR da) {
	return da.length;
}
CHAR DeliteArray_CHAR_apply(DeliteArray_CHAR da, int idx) {
	return da.data[idx];
}
void DeliteArray_CHAR_update(DeliteArray_CHAR da, int idx, CHAR value) {
	da.data[idx] = value;
}

typedef struct {
	int length;
	__global short *data;
} DeliteArray_short;

int DeliteArray_short_size(DeliteArray_short da) {
	return da.length;
}
short DeliteArray_short_apply(DeliteArray_short da, int idx) {
	return da.data[idx];
}
void DeliteArray_short_update(DeliteArray_short da, int idx, short value) {
	da.data[idx] = value;
}

typedef struct {
	int length;
	__global int *data;
} DeliteArray_int;

int DeliteArray_int_size(DeliteArray_int da) {
	return da.length;
}
int DeliteArray_int_apply(DeliteArray_int da, int idx) {
	return da.data[idx];
}
void DeliteArray_int_update(DeliteArray_int da, int idx, int value) {
	da.data[idx] = value;
}

typedef struct {
	int length;
	__global long *data;
} DeliteArray_long;

int DeliteArray_long_size(DeliteArray_long da) {
	return da.length;
}
long DeliteArray_long_apply(DeliteArray_long da, int idx) {
	return da.data[idx];
}
void DeliteArray_long_update(DeliteArray_long da, long idx, int value) {
	da.data[idx] = value;
}


typedef struct {
	int length;
	__global float *data;
} DeliteArray_float;

int DeliteArray_float_size(DeliteArray_float da) {
	return da.length;
}
float DeliteArray_float_apply(DeliteArray_float da, int idx) {
	return da.data[idx];
}
void DeliteArray_float_update(DeliteArray_float da, int idx, float value) {
	da.data[idx] = value;
}


typedef struct {
	int length;
	__global double *data;
} DeliteArray_double;

int DeliteArray_double_size(DeliteArray_double da) {
	return da.length;
}
double DeliteArray_double_apply(DeliteArray_double da, int idx) {
	return da.data[idx];
}
void DeliteArray_double_update(DeliteArray_double da, int idx, double value) {
	da.data[idx] = value;
}

#endif
