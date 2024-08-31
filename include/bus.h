#pragma once
#include <array>
#include <common.h>
#include <olc6502.h>

class Bus {
public:
    // devices.
    olc6502 cpu;
    std::array<u8, 64 * 1024> ram{};

    Bus();
    ~Bus() = default;

    // operations
    u8 read(u16 add, bool ro = false) const;

    void write(u16 add, u8 v);
};
