#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#define CONFIG_ADDR 0xB0000000
#define IBUF_START  0x01000000
#define IBUF_END	  0x01ffffff                               
#define WBUF_START  0x02000000
#define WBUF_END	  0x02ffffff
#define OBUF1_START 0x03000000
#define OBUF1_END		0x037fffff
#define OBUF2_START 0x03800000
#define OBUF2_END		0x03ffffff

typedef struct mem {
	unsigned int opcode : 4;
	unsigned int dep_flags : 4;
	unsigned int dram_address1 : 24;
	unsigned int dram_address2 : 8;
	unsigned int sram_address1 : 24;
	unsigned int sram_address2 : 2;
	unsigned int x_size : 8;
	unsigned int y_size : 8;
	unsigned int z_size : 8;
	unsigned int z_stride1 : 6;
	unsigned int z_stride2 : 2;
	unsigned int y_stride : 8;
	unsigned int is_reset : 1;
	unsigned int bitwidth : 1;
	unsigned int padding : 20;
} Mem_params;

typedef Mem_params Load_params;
typedef Mem_params Store_params;

typedef struct gemm{
	unsigned int opcode : 4;
	unsigned int dep_flags : 4;
	unsigned int input_address : 15;
	unsigned int output_address1 : 9;
	unsigned int output_address2 : 6;
	unsigned int weight_address : 15;
	unsigned int ofmap_height : 8;
	unsigned int ofmap_width1 : 3;
	unsigned int ofmap_width2 : 5;
	unsigned int active_rows : 8, active_cols : 8;
	unsigned int stride_h : 4, stride_w : 4;
	unsigned int pad_left1 : 3;
	unsigned int pad_left2 : 1, pad_right : 4, pad_top : 4, pad_bottom : 4;
	bool preload_output : 1;
	unsigned int padding : 18;
} GEMM_params;

typedef struct alu{
	unsigned int opcode : 4;
	unsigned int dep_flags : 4;
	unsigned int alu_opcode : 2;
	unsigned int input_address : 15;
	unsigned int output_address1 : 7;
	unsigned int output_address2 : 8;
	unsigned int output_height : 8, output_width : 8;
	unsigned int window_height : 4, window_width : 4;
	unsigned int mem_stride_OW : 8, mem_stride_R : 8, mem_stride_S : 8;
	unsigned int num_active : 8;
	bool use_immediate : 1;
	unsigned int immediate_value : 8;
	unsigned int padding : 23;
} ALU_params;
	
int main()
{
	printf("Load: %d\n", sizeof(Load_params));
	printf("Store: %d\n", sizeof(Store_params));
	printf("GEMM: %d\n", sizeof(GEMM_params));
	printf("ALU: %d\n", sizeof(ALU_params));

	uint16_t input[8][8][16];
	uint16_t weight[3][3][16][16];
	uint32_t output[8][8][16];

	for(int i=0; i<8; i++)
		for(int j=0; j<8; j++)
			for(int k=0; k<16; k++)
			{
				input[i][j][k] = i + j + k;
				output[i][j][k] = 0;
			}
	
	for(int r=0; r<3; r++)
		for(int s=0; s<3; s++)
			for(int c=0; c<16; c++)
				for(int m=0; m<16; m++)
					weight[r][s][c][m] = r + s + c + m;

	uint8_t *ins = (uint8_t*) malloc(3 * 128);

	Load_params* ld_1 = (Load_params*) ins;

	ld_1 -> opcode = 8;
	ld_1 -> dep_flags = 0;
	ld_1 -> dram_address1 = ( (uint32_t) input) >> 8;
	ld_1 -> dram_address2 = ((uint32_t) input) & 0xff;
	ld_1 -> sram_address1 = (IBUF_START >> 2);
	ld_1 -> sram_address2 = (IBUF_START & 3);
	ld_1 -> x_size = 8;
	ld_1 -> y_size = 8;
	ld_1 -> z_size = 16;
	ld_1 -> z_stride1 = 16 >> 2;
	ld_1 -> z_stride2 = 16 & 0x3;
	ld_1 -> y_stride = 16;
	ld_1 -> is_reset = 0;
	ld_1 -> bitwidth = 1;

	ld_1 = (Load_params*) ins+16;

	ld_1 -> opcode = 8;
	ld_1 -> dep_flags = 1;
	ld_1 -> dram_address1 = ( (uint32_t) weight) >> 8;
	ld_1 -> dram_address2 = ((uint32_t) weight) & 0xff;
	ld_1 -> sram_address1 = (WBUF_START >> 2);
	ld_1 -> sram_address2 = (WBUF_START & 3);
	ld_1 -> x_size = 1;
	ld_1 -> y_size = 16;
	ld_1 -> z_size = 16;
	ld_1 -> z_stride1 = 16 >> 2;
	ld_1 -> z_stride2 = 16 & 0x3;
	ld_1 -> y_stride = 16;
	ld_1 -> is_reset = 0;
	ld_1 -> bitwidth = 1;

	GEMM_params *gemm_1 = (GEMM_params*) ins + 32;

	gemm_1 -> opcode = 10;
	gemm_1 -> dep_flags = 8;
	gemm_1 -> input_address = IBUF_START >> 4;
	gemm_1 -> output_address1 = OBUF1_START >> (4 + 6);
	gemm_1 -> output_address2 = (OBUF1_START >> 4) & 63;
	gemm_1 -> weight_address = WBUF_START >> 4;
	gemm_1 -> ofmap_height = 8; 
	gemm_1 -> ofmap_width1 = 8 >> 5;
	gemm_1 -> ofmap_width2 = 8 & 31;
	gemm_1 -> active_rows = 16;
	gemm_1 -> active_cols = 16;
	gemm_1 -> stride_h = 1;
	gemm_1 -> stride_w = 1;
	gemm_1 -> pad_left1 = 1 >> 1;
	gemm_1 -> pad_left2 = 1 & 1;
	gemm_1 -> pad_right = 1;
	gemm_1 -> pad_bottom = 1;
	gemm_1 -> pad_top = 1;
	gemm_1 -> preload_output = 0;

	printf("%x %x %x\n", ins, ins+16, ins+32);

	//unsigned long long *config_addr = CONFIG_ADDR;
	//*config_addr = ins;
	//FILE *f = fopen("./code.mem", 'w');
	unsigned char *addr = gemm_1;

	for(int i=0; i<16; i++)
	{
		printf("%.2x\n", *(addr++));
		//printf("%x %d\n", *((unsigned int*)addr), addr);
		//addr ++;
	}

	FILE *f = fopen("../src/code.mem", "w");

	//for(int i=0; i<0x180000; i++)
	//	fprintf(f, "0000000000000000\n");
	// Code mem should have the same width for each line! That's the issue.. Each line is buswidth
	// Yeah every line should be like these 3 I think 
	// There?
	fprintf(f, "80008100004000000202040404100000\n");
	fprintf(f, "81008108008000000044040404100000\n");
	fprintf(f, "A9000000000000404080808800000000\n");
	fprintf(f, "B9000000000808110801001000000000\n");
	fprintf(f, "9800810900C000000202040404000000\n");

	for(int i=0; i<4092; i++)
		fprintf(f, "ab000000000000000000000000000000\n");
	
	unsigned char* wt = (unsigned char*) input;

	for(int i=0; i<128; i++)
	{
		for(int j=0; j<8; j++)
			for(int k=0; k<2; k++)
			{
				fprintf(f, "%.2x", k==0 ? *(wt+1) : *(wt-1));
				wt++;
				//fprintf(f, "%.2x", k==1 ? 0xab : 0xcd);
			}
		fprintf(f, "\n");
	}

	wt = (unsigned char*) weight;
	for(int i=0; i<32; i++)
	{
		for(int j=0; j<8; j++)
			for(int k=0; k<2; k++)
			{
				fprintf(f, "%.2x", k==0 ? *(wt+1) : *(wt-1));
				wt++;
			}
		fprintf(f, "\n");
	}

	for(int i=4256; i<16777216; i++)
		fprintf(f, "%.8x000000000000000000000000\n", i);

	fclose(f);
	return 0;
}
