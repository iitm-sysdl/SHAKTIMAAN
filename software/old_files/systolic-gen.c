#include <stdint.h>

#define IFMAP_ROWDIMS(x) (x<<8)
#define IFMAP_COLDIMS(x) (x<<0)

#define FILTER_ROWDIMS(x) (x<<4)
#define FILTER_COLDIMS(x) (x<<0)

#define INPUT_ROWS 2
#define INPUT_COLS 2

#define NUM_FILTERS 16
#define FILTER_ROWS 2
#define FILTER_COLS 2

#define WEIGHT_NUM (FILTER_ROWS * FILTER_COLS)
#define OUTPUT_NUM (FILTER_ROWS * FILTER_COLS)

void set_systolic(long int* addr, long long int data){
  *addr = data;
}

long long int get_systolic(long long int* addr){
  return *addr;
}

void waitfor(unsigned int secs){
  unsigned int time=0;
  while(time++ < secs);
}

int main()
{
  int* ifmap_dims  = (int*)0xB0000000;
  int* sysConfig   = (int*)0xB0000002;
  int* coordCount  = (int*)0xB0000004;
  int* weightCount = (int*)0xB0000006;
  int* filterDims = (int*)0xB0000008;
  int* input_address = (int*)0xB000000A;
  int* weightStart = (int*)0xB0010000;
  int* gbufStart   = (int*)0xB0030000;

  set_systolic(filterDims, FILTER_ROWDIMS(FILTER_ROWS) | FILTER_COLDIMS(FILTER_COLS));
  set_systolic(ifmap_dims, IFMAP_ROWDIMS(INPUT_ROWS) | IFMAP_COLDIMS(INPUT_COLS));
  set_systolic(weightCount, OUTPUT_NUM);

  int16_t weights[FILTER_ROWS][FILTER_COLS][NUM_FILTERS];
  int16_t input[INPUT_ROWS][INPUT_COLS];
  int16_t output[NUM_FILTERS];

  for(int i=0; i<NUM_FILTERS; i=i+1)
    for(int j=0; j<FILTER_ROWS; j=j+1)
      for(int k=0; k<FILTER_COLS; k=k+1)
        weights[j][k][i] = j*2 + k*3 + i;

  for(int i=0; i<INPUT_ROWS; i++)
    for(int j=0; j<INPUT_COLS; j++)
      input[i][j] = 4*j+i;

  for(int i=0; i<NUM_FILTERS; i++)
    output[i] = 0;

  for(int i=0; i<WEIGHT_NUM; i++)
    for(int j=0; j<NUM_FILTERS/4; j++)
      set_systolic(weightStart+j*4, *((long long int*)&weights[i/FILTER_ROWS][i%FILTER_COLS][j*4]));

  set_systolic(gbufStart, *( (long long int*) &input[0][0]));
  set_systolic(input_address, gbufStart);

  set_systolic(sysConfig, 1);
  
  waitfor(50);
}
