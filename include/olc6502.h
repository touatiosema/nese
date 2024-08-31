#pragma once
#include <map>
#include <string>
#include <vector>
#include <common.h>

class Bus;

class olc6502 {
    Bus *bus = nullptr;

public:
    olc6502();

    ~olc6502() = default;

    // ops
    void connectBus(Bus *bus) {
        this->bus = bus;
    }

    void write(u16 add, u8 v);

    u8 read(u16 add) const;

    // The status register stores 8 flags. Ive enumerated these here for ease
    // of access. You can access the status register directly since its public.
    // The bits have different interpretations depending upon the context and
    // instruction being executed.
    enum FLAGS6502 {
        C = (1 << 0), // Carry Bit
        Z = (1 << 1), // Zero
        I = (1 << 2), // Disable Interrupts
        D = (1 << 3), // Decimal Mode (unused in this implementation)
        B = (1 << 4), // Break
        U = (1 << 5), // Unused
        V = (1 << 6), // Overflow
        N = (1 << 7), // Negative
    };

    // registers
    u8 a = 0x00; // accumulator.
    u8 x = 0x00; // x reg.
    u8 y = 0x00; // y reg.
    u16 pc = 0x00; // program counter.
    u16 stkp = 0x00; // stack pointer.
    u8 status = 0x00; // flags register.

    void clock();

    void reset();

    void irq();

    void nmi();

    u8 fetch();

    u8 fetched = 0x00;
    u16 addr_abs = 0x0000;
    u16 addr_rel = 0x00;
    u8 opcode = 0x00;
    u8 cycles = 0;
    u32 clock_count = 0;
    u16 temp = 0x0000; // A convenience variable used everywhere

    struct INSTRUCTION {
        std::string name;

        u8 (olc6502::*operate)(void) = nullptr;

        u8 (olc6502::*addrmode)(void) = nullptr;

        u8 cycles = 0;
    };

    std::vector<INSTRUCTION> lookup;

    bool complete();

    std::map<u16, std::string> disassemble(u16 nStart, u16 nStop);

private:
    u8 getFlag(FLAGS6502 f) const;

    void setFlag(FLAGS6502 f, bool v);

    // Addressing Modes =============================================
    // The 6502 has a variety of addressing modes to access data in
    // memory, some of which are direct and some are indirect (like
    // pointers in C++). Each opcode contains information about which
    // addressing mode should be employed to facilitate the
    // instruction, in regards to where it reads/writes the data it
    // uses. The address mode changes the number of bytes that
    // makes up the full instruction, so we implement addressing
    // before executing the instruction, to make sure the program
    // counter is at the correct location, the instruction is
    // primed with the addresses it needs, and the number of clock
    // cycles the instruction requires is calculated. These functions
    // may adjust the number of cycles required depending upon where
    // and how the memory is accessed, so they return the required
    // adjustment.

    u8 IMP();

    u8 IMM();

    u8 ZP0();

    u8 ZPX();

    u8 ZPY();

    u8 REL();

    u8 ABS();

    u8 ABX();

    u8 ABY();

    u8 IND();

    u8 IZX();

    u8 IZY();

    // Opcodes ======================================================
    // There are 56 "legitimate" opcodes provided by the 6502 CPU. I
    // have not modelled "unofficial" opcodes. As each opcode is
    // defined by 1 byte, there are potentially 256 possible codes.
    // Codes are not used in a "switch case" style on a processor,
    // instead they are repsonisble for switching individual parts of
    // CPU circuits on and off. The opcodes listed here are official,
    // meaning that the functionality of the chip when provided with
    // these codes is as the developers intended it to be. Unofficial
    // codes will of course also influence the CPU circuitry in
    // interesting ways, and can be exploited to gain additional
    // functionality!
    //
    // These functions return 0 normally, but some are capable of
    // requiring more clock cycles when executed under certain
    // conditions combined with certain addressing modes. If that is
    // the case, they return 1.
    //
    // I have included detailed explanations of each function in
    // the class implementation file. Note they are listed in
    // alphabetical order here for ease of finding.

    u8 ADC();

    u8 AND();

    u8 ASL();

    u8 BCC();

    u8 BCS();

    u8 BEQ();

    u8 BIT();

    u8 BMI();

    u8 BNE();

    u8 BPL();

    u8 BRK();

    u8 BVC();

    u8 BVS();

    u8 CLC();

    u8 CLD();

    u8 CLI();

    u8 CLV();

    u8 CMP();

    u8 CPX();

    u8 CPY();

    u8 DEC();

    u8 DEX();

    u8 DEY();

    u8 EOR();

    u8 INC();

    u8 INX();

    u8 INY();

    u8 JMP();

    u8 JSR();

    u8 LDA();

    u8 LDX();

    u8 LDY();

    u8 LSR();

    u8 NOP();

    u8 ORA();

    u8 PHA();

    u8 PHP();

    u8 PLA();

    u8 PLP();

    u8 ROL();

    u8 ROR();

    u8 RTI();

    u8 RTS();

    u8 SBC();

    u8 SEC();

    u8 SED();

    u8 SEI();

    u8 STA();

    u8 STX();

    u8 STY();

    u8 TAX();

    u8 TAY();

    u8 TSX();

    u8 TXA();

    u8 TXS();

    u8 TYA();

    // I capture all "unofficial" opcodes with this function. It is
    // functionally identical to a NOP
    u8 XXX();
};
