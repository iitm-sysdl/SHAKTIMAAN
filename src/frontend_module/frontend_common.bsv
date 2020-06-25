/* 
Author: Vinod Ganesan
Email id: g.vinod1993@gmail.com
Details:
--------------------------------------------------------------------------------------------------
*/

package frontend_common;


//typedefs
//We may not instantiate >1 accelerator at a time, however in-case that has to be done, this needs 
//to change to a parametric type
typedef 8 TloadQdepth;
typedef 8 TcomputeQdepth;
typedef 8 TstoreQdepth;
typedef 8 TaluQdepth;
typedef 8 G2lQdepth;
typedef 8 L2gQdepth;
typedef 8 G2aQdepth;
typedef 8 A2gQdepth;
typedef 8 S2aQdepth;
typedef 8 A2sQdepth;
typedef 8 LoadQdepth;
typedef 8 StoreQdepth;
typedef 8 ComputeQdepth;
typedef 8 AluQdepth;
typedef 8 InsSize;
typedef 4 OpWidth;

//Instruction typedefs
typedef 128 ILEN;
typedef 4 Opcode;
typedef 4 Dept;

// push_prev_dep |  push_next_dep | pop_prev_dep | pop_next_dep


endpackage
