const std = @import("std");

pub fn main() anyerror!void {
    std.log.info("All your codebase are belong to us.", .{});
}


const AddrOrValue = union(enum) {
    addr: u8,
    val: i32,
};

const Copy = struct{
    from: AddrOrValue,
    to: u8,
};

const Jump = struct {
    condition: AddrOrValue,
    dir: i32,
}:

const Instruction = union(enum) {
    copy: Copy,
    inc: u8,
    dec: u8,
    jump: Jump,
};

fn parseD12(lines: []const u8) []Instruction {

}

fn parseLineD12(line: []const u8) Instruction {
    switch (line[0]) {
        'c' => {},
        'i' => {},
        'd' => {},
        'j' => {}
    }
}

test "parse day 12" {
    const lines = [_][]const u8{
        "cpy 41 a",
        "inc a",
        "inc a",
        "dec a",
        "jnz a 2",
        "dec a",
    };


}

pub fn day12() !void {
    // cpy x y copies x (either an integer or the value of a register) into register y.
    // inc x increases the value of register x by one.
    // dec x decreases the value of register x by one.
    // jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.


}