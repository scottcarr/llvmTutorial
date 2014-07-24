CXX=clang++
CXXFLAG=-std=c++11 -g 
#LLVMFLAG:=`/usr/bin/llvm-config-3.4 --cppflags --ldflags --libs all`
LLVMFLAG:=`llvm-config --cppflags --ldflags --libs all`
#LLVMFLAG:=`~/llvm/build/Debug+Asserts/bin/llvm-config --cppflags --ldflags --libs all`
#LLVMFLAG:=`llvm-config --cppflags --ldflags --libs all`
LIBS=-lpthread -ldl -lcurses -lz

all: toy.cpp
	$(CXX) $(CXXFLAG) toy.cpp $(LLVMFLAG) $(LIBS) -o toy 

