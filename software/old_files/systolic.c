#include <stdint.h>


#define IFMAP_ROWDIMS(x) (x<<8)
#define IFMAP_COLDIMS(x) (x<<0)

void set_systolic(long int* addr, long long int data){
  *addr = data;
}

long long int get_systolic(long long int* addr){
  return *addr;
}

void waitfor(unsigned int secs) {
	unsigned int time = 0;
	while(time++ < secs);
}

int main(){
  
  int* ifmap_dims  = (int*)0xB0000000;
  int* sysConfig   = (int*)0xB0000002;
  int* coordCount  = (int*)0xB0000004;
  int* weightCount = (int*)0xB0000006;
  int* weightStart = (int*)0xB0010000;
  int* gbufStart   = (int*)0xB0030000;

  set_systolic(coordCount, 1); //Setting coordinate counter to be one -- This might not be req!
  set_systolic(weightCount,4); //Setting weight count to be 4
  set_systolic(ifmap_dims, IFMAP_ROWDIMS(2)|IFMAP_COLDIMS(2));
  //int16_t weightdata[4][2][2] = {0};
  int16_t weightChan[2][2][4] = {0}; //Filter Major
  int16_t accumData[2][2] = {0}; //Just trying out convolution with one fmap for now

//  int16_t ifmap[2][2] = { {2,3}, 
//                          {4,5} 
//                        };
//
  int16_t ifmaptrans[2][2] = { {2,4},
                               {3,5}
                             };

  //initializing weight vector and accumulation vector
  // This will be stored in Row major order!!
  //for(int i = 0; i < 4; i++){
  //  for(int j = 0; j < 2 ; j++){
  //    for(int k = 0; k < 2; k++){
  //        weightdata[i][j][k] = i*4+j*2+k;
  //        accumdata[i][j][k]  = 0; //Assume zero for now
  //    }
  //  }
  //}
 
  //Value in Different way of storing
  for(int i = 0; i < 2; i++)
    for(int j = 0; j < 2; j++)
      for(int k = 0; k < 4; k++)
        weightChan[i][j][k] = k*4+j*2+i;  //Same value as above stored differently

  int addr = 0;
  long long int *weight ;

  //Loading Weight buffers
  for(int i = 0; i < 2; i++){
    for(int j = 0; j < 2; j++){
      weight = (long int*)&weightChan[i][j][0];
      //for(int k = 0; k < 4; k++){
      //  weight = (weight << 16) | (long long int)weightdata[k][i][j]; // -- This should be 1 cycle
      //printf("\t WData: %016x weight: %016x \n", weightChan[i][j][0], *weight);
      //}
      set_systolic(weightStart, *weight);
      weight = 0;
    }
  }

  int* activations;
  //Loading Global Buffers
  //initializing global buffers
  for(int i = 0 ; i < 2; i++){
    activations = (int*)&ifmaptrans[i][0];
    set_systolic(gbufStart, *activations);
    gbufStart+=1;
  }

  set_systolic(sysConfig, 1);

  waitfor(50);

  return 0;
}
