#include <olc6502.h>
#include <bus.h>
#include <map>

olc6502::olc6502() {
    using a = olc6502;
    lookup =
    {
        {"BRK", &a::BRK, &a::IMM, 7}, {"ORA", &a::ORA, &a::IZX, 6}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"???", &a::NOP, &a::IMP, 3}, {"ORA", &a::ORA, &a::ZP0, 3},
        {"ASL", &a::ASL, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5}, {"PHP", &a::PHP, &a::IMP, 3},
        {"ORA", &a::ORA, &a::IMM, 2}, {"ASL", &a::ASL, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::NOP, &a::IMP, 4}, {"ORA", &a::ORA, &a::ABS, 4}, {"ASL", &a::ASL, &a::ABS, 6},
        {"???", &a::XXX, &a::IMP, 6},
        {"BPL", &a::BPL, &a::REL, 2}, {"ORA", &a::ORA, &a::IZY, 5}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"???", &a::NOP, &a::IMP, 4}, {"ORA", &a::ORA, &a::ZPX, 4},
        {"ASL", &a::ASL, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6}, {"CLC", &a::CLC, &a::IMP, 2},
        {"ORA", &a::ORA, &a::ABY, 4}, {"???", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
        {"???", &a::NOP, &a::IMP, 4}, {"ORA", &a::ORA, &a::ABX, 4}, {"ASL", &a::ASL, &a::ABX, 7},
        {"???", &a::XXX, &a::IMP, 7},
        {"JSR", &a::JSR, &a::ABS, 6}, {"AND", &a::AND, &a::IZX, 6}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"BIT", &a::BIT, &a::ZP0, 3}, {"AND", &a::AND, &a::ZP0, 3},
        {"ROL", &a::ROL, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5}, {"PLP", &a::PLP, &a::IMP, 4},
        {"AND", &a::AND, &a::IMM, 2}, {"ROL", &a::ROL, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
        {"BIT", &a::BIT, &a::ABS, 4}, {"AND", &a::AND, &a::ABS, 4}, {"ROL", &a::ROL, &a::ABS, 6},
        {"???", &a::XXX, &a::IMP, 6},
        {"BMI", &a::BMI, &a::REL, 2}, {"AND", &a::AND, &a::IZY, 5}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"???", &a::NOP, &a::IMP, 4}, {"AND", &a::AND, &a::ZPX, 4},
        {"ROL", &a::ROL, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6}, {"SEC", &a::SEC, &a::IMP, 2},
        {"AND", &a::AND, &a::ABY, 4}, {"???", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
        {"???", &a::NOP, &a::IMP, 4}, {"AND", &a::AND, &a::ABX, 4}, {"ROL", &a::ROL, &a::ABX, 7},
        {"???", &a::XXX, &a::IMP, 7},
        {"RTI", &a::RTI, &a::IMP, 6}, {"EOR", &a::EOR, &a::IZX, 6}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"???", &a::NOP, &a::IMP, 3}, {"EOR", &a::EOR, &a::ZP0, 3},
        {"LSR", &a::LSR, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5}, {"PHA", &a::PHA, &a::IMP, 3},
        {"EOR", &a::EOR, &a::IMM, 2}, {"LSR", &a::LSR, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
        {"JMP", &a::JMP, &a::ABS, 3}, {"EOR", &a::EOR, &a::ABS, 4}, {"LSR", &a::LSR, &a::ABS, 6},
        {"???", &a::XXX, &a::IMP, 6},
        {"BVC", &a::BVC, &a::REL, 2}, {"EOR", &a::EOR, &a::IZY, 5}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"???", &a::NOP, &a::IMP, 4}, {"EOR", &a::EOR, &a::ZPX, 4},
        {"LSR", &a::LSR, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6}, {"CLI", &a::CLI, &a::IMP, 2},
        {"EOR", &a::EOR, &a::ABY, 4}, {"???", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
        {"???", &a::NOP, &a::IMP, 4}, {"EOR", &a::EOR, &a::ABX, 4}, {"LSR", &a::LSR, &a::ABX, 7},
        {"???", &a::XXX, &a::IMP, 7},
        {"RTS", &a::RTS, &a::IMP, 6}, {"ADC", &a::ADC, &a::IZX, 6}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"???", &a::NOP, &a::IMP, 3}, {"ADC", &a::ADC, &a::ZP0, 3},
        {"ROR", &a::ROR, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5}, {"PLA", &a::PLA, &a::IMP, 4},
        {"ADC", &a::ADC, &a::IMM, 2}, {"ROR", &a::ROR, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
        {"JMP", &a::JMP, &a::IND, 5}, {"ADC", &a::ADC, &a::ABS, 4}, {"ROR", &a::ROR, &a::ABS, 6},
        {"???", &a::XXX, &a::IMP, 6},
        {"BVS", &a::BVS, &a::REL, 2}, {"ADC", &a::ADC, &a::IZY, 5}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"???", &a::NOP, &a::IMP, 4}, {"ADC", &a::ADC, &a::ZPX, 4},
        {"ROR", &a::ROR, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6}, {"SEI", &a::SEI, &a::IMP, 2},
        {"ADC", &a::ADC, &a::ABY, 4}, {"???", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
        {"???", &a::NOP, &a::IMP, 4}, {"ADC", &a::ADC, &a::ABX, 4}, {"ROR", &a::ROR, &a::ABX, 7},
        {"???", &a::XXX, &a::IMP, 7},
        {"???", &a::NOP, &a::IMP, 2}, {"STA", &a::STA, &a::IZX, 6}, {"???", &a::NOP, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 6}, {"STY", &a::STY, &a::ZP0, 3}, {"STA", &a::STA, &a::ZP0, 3},
        {"STX", &a::STX, &a::ZP0, 3}, {"???", &a::XXX, &a::IMP, 3}, {"DEY", &a::DEY, &a::IMP, 2},
        {"???", &a::NOP, &a::IMP, 2}, {"TXA", &a::TXA, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
        {"STY", &a::STY, &a::ABS, 4}, {"STA", &a::STA, &a::ABS, 4}, {"STX", &a::STX, &a::ABS, 4},
        {"???", &a::XXX, &a::IMP, 4},
        {"BCC", &a::BCC, &a::REL, 2}, {"STA", &a::STA, &a::IZY, 6}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 6}, {"STY", &a::STY, &a::ZPX, 4}, {"STA", &a::STA, &a::ZPX, 4},
        {"STX", &a::STX, &a::ZPY, 4}, {"???", &a::XXX, &a::IMP, 4}, {"TYA", &a::TYA, &a::IMP, 2},
        {"STA", &a::STA, &a::ABY, 5}, {"TXS", &a::TXS, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 5},
        {"???", &a::NOP, &a::IMP, 5}, {"STA", &a::STA, &a::ABX, 5}, {"???", &a::XXX, &a::IMP, 5},
        {"???", &a::XXX, &a::IMP, 5},
        {"LDY", &a::LDY, &a::IMM, 2}, {"LDA", &a::LDA, &a::IZX, 6}, {"LDX", &a::LDX, &a::IMM, 2},
        {"???", &a::XXX, &a::IMP, 6}, {"LDY", &a::LDY, &a::ZP0, 3}, {"LDA", &a::LDA, &a::ZP0, 3},
        {"LDX", &a::LDX, &a::ZP0, 3}, {"???", &a::XXX, &a::IMP, 3}, {"TAY", &a::TAY, &a::IMP, 2},
        {"LDA", &a::LDA, &a::IMM, 2}, {"TAX", &a::TAX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
        {"LDY", &a::LDY, &a::ABS, 4}, {"LDA", &a::LDA, &a::ABS, 4}, {"LDX", &a::LDX, &a::ABS, 4},
        {"???", &a::XXX, &a::IMP, 4},
        {"BCS", &a::BCS, &a::REL, 2}, {"LDA", &a::LDA, &a::IZY, 5}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 5}, {"LDY", &a::LDY, &a::ZPX, 4}, {"LDA", &a::LDA, &a::ZPX, 4},
        {"LDX", &a::LDX, &a::ZPY, 4}, {"???", &a::XXX, &a::IMP, 4}, {"CLV", &a::CLV, &a::IMP, 2},
        {"LDA", &a::LDA, &a::ABY, 4}, {"TSX", &a::TSX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 4},
        {"LDY", &a::LDY, &a::ABX, 4}, {"LDA", &a::LDA, &a::ABX, 4}, {"LDX", &a::LDX, &a::ABY, 4},
        {"???", &a::XXX, &a::IMP, 4},
        {"CPY", &a::CPY, &a::IMM, 2}, {"CMP", &a::CMP, &a::IZX, 6}, {"???", &a::NOP, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"CPY", &a::CPY, &a::ZP0, 3}, {"CMP", &a::CMP, &a::ZP0, 3},
        {"DEC", &a::DEC, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5}, {"INY", &a::INY, &a::IMP, 2},
        {"CMP", &a::CMP, &a::IMM, 2}, {"DEX", &a::DEX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
        {"CPY", &a::CPY, &a::ABS, 4}, {"CMP", &a::CMP, &a::ABS, 4}, {"DEC", &a::DEC, &a::ABS, 6},
        {"???", &a::XXX, &a::IMP, 6},
        {"BNE", &a::BNE, &a::REL, 2}, {"CMP", &a::CMP, &a::IZY, 5}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"???", &a::NOP, &a::IMP, 4}, {"CMP", &a::CMP, &a::ZPX, 4},
        {"DEC", &a::DEC, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6}, {"CLD", &a::CLD, &a::IMP, 2},
        {"CMP", &a::CMP, &a::ABY, 4}, {"NOP", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
        {"???", &a::NOP, &a::IMP, 4}, {"CMP", &a::CMP, &a::ABX, 4}, {"DEC", &a::DEC, &a::ABX, 7},
        {"???", &a::XXX, &a::IMP, 7},
        {"CPX", &a::CPX, &a::IMM, 2}, {"SBC", &a::SBC, &a::IZX, 6}, {"???", &a::NOP, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"CPX", &a::CPX, &a::ZP0, 3}, {"SBC", &a::SBC, &a::ZP0, 3},
        {"INC", &a::INC, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5}, {"INX", &a::INX, &a::IMP, 2},
        {"SBC", &a::SBC, &a::IMM, 2}, {"NOP", &a::NOP, &a::IMP, 2}, {"???", &a::SBC, &a::IMP, 2},
        {"CPX", &a::CPX, &a::ABS, 4}, {"SBC", &a::SBC, &a::ABS, 4}, {"INC", &a::INC, &a::ABS, 6},
        {"???", &a::XXX, &a::IMP, 6},
        {"BEQ", &a::BEQ, &a::REL, 2}, {"SBC", &a::SBC, &a::IZY, 5}, {"???", &a::XXX, &a::IMP, 2},
        {"???", &a::XXX, &a::IMP, 8}, {"???", &a::NOP, &a::IMP, 4}, {"SBC", &a::SBC, &a::ZPX, 4},
        {"INC", &a::INC, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6}, {"SED", &a::SED, &a::IMP, 2},
        {"SBC", &a::SBC, &a::ABY, 4}, {"NOP", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
        {"???", &a::NOP, &a::IMP, 4}, {"SBC", &a::SBC, &a::ABX, 4}, {"INC", &a::INC, &a::ABX, 7},
        {"???", &a::XXX, &a::IMP, 7},
    };
}

u8 olc6502::read(u16 add) const {
    return this->bus->read(add);
}

void olc6502::write(u16 add, u8 v) {
    this->bus->write(add, v);
}

// Forces the 6502 into a known state. This is hard-wired inside the CPU.
// An absolute address is read from location 0xFFFC
// which contains a second address that the program counter is set to. This
// allows the programmer to jump to a known and programmable location in the
// memory to start executing from. Typically the programmer would set the value
// at location 0xFFFC at compile time.
void olc6502::reset() {
    a = 0x00;
    x = 0x00;
    y = 0x00;
    status = 0x00 | U;
    stkp = 0xfd;
    pc = read(0xfffc) | (read(0xfffd) << 8);

    // clearing internals
    addr_abs = 0x0000;
    addr_rel = 0x00;
    fetched = 0x00;

    // waiting appropriate cycles.
    cycles = 8;
}

// interruption request, only if I flag is off.
void olc6502::irq() {
    if (getFlag(I) == 0) {
        write(0x100 + stkp, (pc >> 8) & 0x00ff);
        stkp--;
        write(0x100 + stkp, pc & 0x00ff);
        stkp--;

        setFlag(B, 0);
        setFlag(U, 1);
        setFlag(I, 1);
        write(0x100 + stkp, status);
        stkp--;

        pc = read(0xfffa) | (read(0xfffb) << 8);
        cycles = 8;
    }
}

// A Non-Maskable Interrupt cannot be ignored. It behaves in exactly the
// same way as a regular IRQ, but reads the new program counter address
// form location 0xFFFA.
void olc6502::nmi() {
    write(0x0100 + stkp, (pc >> 8) & 0x00FF);
    stkp--;
    write(0x0100 + stkp, pc & 0x00FF);
    stkp--;

    setFlag(B, 0);
    setFlag(U, 1);
    setFlag(I, 1);
    write(0x0100 + stkp, status);
    stkp--;

    addr_abs = 0xFFFA;
    u16 lo = read(0xfffa + 0);
    u16 hi = read(0xfffb);
    pc = (hi << 8) | lo;

    cycles = 8;
}

void olc6502::clock() {
    if (cycles == 0) {
        opcode = read(pc++);
        setFlag(U, true);
        cycles = lookup[opcode].cycles;
        // we have additional cycles if both the operate and addmode requires additional cycles.
        u8 additional_cycles1 = (this->*lookup[opcode].addrmode)();
        u8 additional_cycles2 = (this->*lookup[opcode].operate)();
        cycles += (additional_cycles1 & additional_cycles2);
    }
    clock_count++;
    cycles--;
}

void olc6502::setFlag(FLAGS6502 f, bool v) {
    if (v) {
        status |= f;
    } else {
        status &= ~f;
    }
}

u8 olc6502::getFlag(FLAGS6502 f) const {
    return (status & f) ? 1 : 0;
}

///////////////////////////////////////////////////////////////////////////////
// HELPER FUNCTIONS
bool olc6502::complete() {
    return cycles == 0;
}

// This is the disassembly function. Its workings are not required for emulation.
// It is merely a convenience function to turn the binary instruction code into
// human readable form. Its included as part of the emulator because it can take
// advantage of many of the CPUs internal operations to do this.
std::map<u16, std::string> olc6502::disassemble(u16 nStart, u16 nStop) {
    u32 addr = nStart;
    u8 value = 0x00, lo = 0x00, hi = 0x00;
    std::map<u16, std::string> mapLines;
    u16 line_addr = 0;

    // A convenient utility to convert variables into
    // hex strings because "modern C++"'s method with
    // streams is atrocious
    auto hex = [](u32 n, u8 d) {
        std::string s(d, '0');
        for (int i = d - 1; i >= 0; i--, n >>= 4)
            s[i] = "0123456789ABCDEF"[n & 0xF];
        return s;
    };

    // Starting at the specified address we read an instruction
    // byte, which in turn yields information from the lookup table
    // as to how many additional bytes we need to read and what the
    // addressing mode is. I need this info to assemble human readable
    // syntax, which is different depending upon the addressing mode

    // As the instruction is decoded, a std::string is assembled
    // with the readable output
    while (addr <= (u32) nStop) {
        line_addr = addr;

        // Prefix line with instruction address
        std::string sInst = "$" + hex(addr, 4) + ": ";

        // Read instruction, and get its readable name
        u8 opcode = bus->read(addr, true);
        addr++;
        sInst += lookup[opcode].name + " ";

        // Get oprands from desired locations, and form the
        // instruction based upon its addressing mode. These
        // routines mimmick the actual fetch routine of the
        // 6502 in order to get accurate data as part of the
        // instruction
        if (lookup[opcode].addrmode == &olc6502::IMP) {
            sInst += " {IMP}";
        } else if (lookup[opcode].addrmode == &olc6502::IMM) {
            value = bus->read(addr, true);
            addr++;
            sInst += "#$" + hex(value, 2) + " {IMM}";
        } else if (lookup[opcode].addrmode == &olc6502::ZP0) {
            lo = bus->read(addr, true);
            addr++;
            hi = 0x00;
            sInst += "$" + hex(lo, 2) + " {ZP0}";
        } else if (lookup[opcode].addrmode == &olc6502::ZPX) {
            lo = bus->read(addr, true);
            addr++;
            hi = 0x00;
            sInst += "$" + hex(lo, 2) + ", X {ZPX}";
        } else if (lookup[opcode].addrmode == &olc6502::ZPY) {
            lo = bus->read(addr, true);
            addr++;
            hi = 0x00;
            sInst += "$" + hex(lo, 2) + ", Y {ZPY}";
        } else if (lookup[opcode].addrmode == &olc6502::IZX) {
            lo = bus->read(addr, true);
            addr++;
            hi = 0x00;
            sInst += "($" + hex(lo, 2) + ", X) {IZX}";
        } else if (lookup[opcode].addrmode == &olc6502::IZY) {
            lo = bus->read(addr, true);
            addr++;
            hi = 0x00;
            sInst += "($" + hex(lo, 2) + "), Y {IZY}";
        } else if (lookup[opcode].addrmode == &olc6502::ABS) {
            lo = bus->read(addr, true);
            addr++;
            hi = bus->read(addr, true);
            addr++;
            sInst += "$" + hex((u16) (hi << 8) | lo, 4) + " {ABS}";
        } else if (lookup[opcode].addrmode == &olc6502::ABX) {
            lo = bus->read(addr, true);
            addr++;
            hi = bus->read(addr, true);
            addr++;
            sInst += "$" + hex((u16) (hi << 8) | lo, 4) + ", X {ABX}";
        } else if (lookup[opcode].addrmode == &olc6502::ABY) {
            lo = bus->read(addr, true);
            addr++;
            hi = bus->read(addr, true);
            addr++;
            sInst += "$" + hex((u16) (hi << 8) | lo, 4) + ", Y {ABY}";
        } else if (lookup[opcode].addrmode == &olc6502::IND) {
            lo = bus->read(addr, true);
            addr++;
            hi = bus->read(addr, true);
            addr++;
            sInst += "($" + hex((u16) (hi << 8) | lo, 4) + ") {IND}";
        } else if (lookup[opcode].addrmode == &olc6502::REL) {
            value = bus->read(addr, true);
            addr++;
            sInst += "$" + hex(value, 2) + " [$" + hex(addr + value, 4) + "] {REL}";
        }

        // Add the formed string to a std::map, using the instruction's
        // address as the key. This makes it convenient to look for later
        // as the instructions are variable in length, so a straight up
        // incremental index is not sufficient.
        mapLines[line_addr] = sInst;
    }

    return mapLines;
}
