# caution: this makefile does NOT check for changes in header files! i.e. the "clean" target may need to be invoked manually

# for debugging it may be usefull to add these to gcc/g++: -g -rdynamic -funwind-tables
CC = gcc -fnon-call-exceptions
CXX = g++ -fnon-call-exceptions -std=c++11

CFLAGS =  -DRPI4 -Wno-pointer-sign -Wall -c 
CXXFLAGS = -DRPI4 -fno-rtti -Wall -D__STDC_LIMIT_MACROS

LDFLAGS = -funroll-loops -Ofast

SRCDIR=../src
RSRCDIR=./src

OBJDIR = ./obj

CCOBJS = $(OBJDIR)/cia.o $(OBJDIR)/cpu.o $(OBJDIR)/hacks.o $(OBJDIR)/memory.o $(OBJDIR)/vic.o
CXXOBJS = $(OBJDIR)/core.o $(OBJDIR)/digi.o $(OBJDIR)/envelope.o $(OBJDIR)/filter.o $(OBJDIR)/loaders.o $(OBJDIR)/sid.o $(OBJDIR)/system.o $(OBJDIR)/wavegenerator.o $(OBJDIR)/sidplayer.o 
CXXROBJS = $(OBJDIR)/main.o $(OBJDIR)/rpi4_utils.o $(OBJDIR)/gpio.o $(OBJDIR)/cp1252.o

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	$(CC) -c -o $@ $< $(CFLAGS)

$(OBJDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -c -o $@ $< $(CXXFLAGS)

$(OBJDIR)/%.o: $(RSRCDIR)/%.cpp
	$(CXX) -c -o $@ $< $(CXXFLAGS)

all: websid

websid: $(CCOBJS) $(CXXOBJS) $(CXXROBJS)
		 g++ $(LDFLAGS)  $(CCOBJS) $(CXXOBJS) $(CXXROBJS) -lm -lpthread -lwiringPi -o websid

		 
clean:
	rm -f $(OBJDIR)/*.o
	rm -f websid