PRJ = Top
JAVA_HOME = /usr/lib/jvm/java-7-openjdk-amd64
BUILD_CARD = GenericDevice

SOURCES                 =       $(wildcard chisel/src/runtime/*.cpp cpp/src/kernels/*.cpp cpp/src/static/cpp/*.cpp)
INCLUDES 		=	-Icpp/src/datastructures -Icpp/src/static/cpp -Icpp/src/kernels -Icpp/src/runtime -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/linux
CC 				=	gcc
CXX 			=	g++
CFLAGS 			=	-g3 -D__DELITE_CPP_STANDALONE__ -fPIC -Wall -Wno-sign-compare -Wno-unused-variable -Wno-return-type $(INCLUDES)
LDFLAGS 		=	-Wl,--hash-style=both -lstdc++ -lpthread -lm
# $(info $$var is [${SOURCES}])

#SIM_ID			=	$$(cat /dev/urandom | tr -dc 'a-zA-Z' | fold -w 5 | head -n 1) # Recomputes @ compile time and runtime 
#SIM_ID			=	$$(ls -i . | grep Makefile | sed 's/ Makefile//g') # Won't work if copying data to maxeler
SIM_ID			=   $(PRJ)$$(md5sum ./maxj/src/kernels/TopKernelLib.maxj | cut -c1-5 | sed 's/1/z/g' | sed 's/2/y/g' | sed 's/3/q/g' | sed 's/4/r/g' | sed 's/5/s/g' | sed 's/6/t/g' | sed 's/7/u/g' | sed 's/8/v/g' | sed 's/9/w/g' | sed 's/0/x/g')

SIM_PREFIX              = SIM
FPGA_PREFIX              = FPGA
#MAXFILE_SIM_OBJ		=	maxfile_$(PRJ)_Sim$(BUILD_TAG).o
#MAXFILE_FPGA_OBJ		=	maxfile_$(PRJ)_FPGA$(BUILD_TAG).o
CHISEL_SIM_OBJ		=	$(PRJ)_$(SIM_PREFIX).o
CHISEL_FPGA_OBJ		=	$(PRJ)_$(FPGA_PREFIX).o

CHISEL_SIM_EXE		=	$(PRJ)_sim
CHISEL_FPGA_EXE		=	$(PRJ)_fpga

OUT_DIR_$(SIM_PREFIX)             =       $(PRJ)_$(BUILD_CARD)_$(SIM_PREFIX)
OUT_DIR_$(DFE_PREFIX)             =       $(PRJ)_$(BUILD_CARD)_$(DFE_PREFIX)


help:
	@echo "Targets:"
	@echo "    verilog-sim  Compile verilog for simulation"
	@echo "    verilog-fpga  Compile verilog for hardware bitstream"
	@echo "    cpu-sim    Compile the CPU code for simulation"
	@echo "    cpu-fpga    Compile the CPU code for hardware"
	@echo "    run-sim        Run the simulation process"
	@echo "    stop-sim        Run the simulation process"
	@echo "    run-fpga        Run the hardware test"
	@echo "    stop-fpga       Stop the hardware test"
	@echo "    clean-sim      Delete all files for simulation"
	@echo "    clean          Delete all files (simulation and hardware)"

cpu-sim: $(CHISEL_SIM_EXE)
cpu-fpga: $(CHISEL_DFE_EXE)

bitstream-sim:
	sbt compile

bitstream-fpga:

$(CHISEL_SIM_EXE): $(SOURCES)
	$(CC) -I$(PRJ)_$(SIM_PREFIX)/results -rdynamic -I$(OUT_DIR_$(SIM_PREFIX))/results $(CFLAGS) -o $@ $^ $(LDFLAGS)
	#$(CC) -I$(PRJ)_Sim$(BUILD_TAG)/results -rdynamic $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(CHISEL_DFE_EXE): $(CHISEL_DFE_OBJ) $(SOURCES)
	$(CC) -I$(PRJ)_$(DFE_PREFIX)/results -rdynamic $(CFLAGS) -I$(OUT_DIR_$(DFE_PREFIX))/results -o $@ $^ $(LDFLAGS)
	#$(CC) -I$(PRJ)_DFE$(BUILD_TAG)/results -rdynamic $(CFLAGS) -o $@ $^ $(LDFLAGS)

run-sim: $(CHISEL_SIM_EXE)

stop-sim:
	$(MAXCOMPILERDIR)/bin/maxcompilersim -c $(BUILD_CARD) -n $(SIM_ID) stop

run-fpga: $(CHISEL_DFE_EXE)
	# LD_LIBRARY_PATH="$(MAXELEROSDIR)/lib/" SLIC_CONF="$(SLIC_CONF_DFE)" ./$< $(PRJ)_DFE$(BUILD_TAG) $(MAIA_FLAG)
	LD_LIBRARY_PATH="$(MAXELEROSDIR)/lib/" SLIC_CONF="$(SLIC_CONF_DFE)" ./$< ${ARGS}

stop-fpga:
ifeq ($(MAIA_FLAG), 1)
	/opt/maxeler/maxeleros/utils/maxforceidle -a -r 192.168.0.10
else
	/opt/maxeler/maxeleros/utils/maxforceidle -a
endif

clean:
	-$(MAXCOMPILERDIR)/bin/maxcompilersim -n $(SIM_ID) stop
	rm -f $(CHISEL_SIM_OBJ)
	rm -f $(CHISEL_SIM_EXE)
	rm -rf bin $(OUT_DIR_$(SIM_PREFIX))
	rm -f $(OBJECTS)
	rm -f *.log
	rm -rf debug*

clean-all:
	-$(MAXCOMPILERDIR)/bin/maxcompilersim -n $(SIM_ID) stop
	rm -f *.o 
	rm -f $(CHISEL_SIM_EXE) $(CHISEL_DFE_EXE)
	rm -rf bin $(OUT_DIR_$(SIM_PREFIX)) $(OUT_DIR_$(DFE_PREFIX))
	rm -f $(OBJECTS)
	rm -f *.log
	rm -rf debug*
	#rm -r bin $(PRJ)_Sim $(PRJ)_DFE$(BUILD_TAG)

sim: bitstream-sim cpu-sim
fpga: bitstream-fpga cpu-fpga
