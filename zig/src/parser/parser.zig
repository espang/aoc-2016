const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Error = error {
    EndOfStream,
} || std.mem.Allocator.Error;

pub fn Parser(comptime Value: type, comptime Reader: type) type {
    return struct {
        const Self = @This();
        _parse: fn(self: *Self, allocator: *Allocator, src: *Reader) callconv(.Inline) Error!?Value,

        pub fn parse(self: *Self, allocator: *Allocator, src: *Reader) callconv(.Inline) Error!?Value {
            return self._parse(self, allocator, src);
        }
    };
}

pub fn Literal(comptime Reader: type) type {
    return struct {
        parser: Parser([]u8, Reader) = .{
           ._parse = parse,
        },
        want: []const u8,

        const Self = @This();

        pub fn init(want: []const u8) Self {
            return Self{
                .want = want
            };
        }

        fn parse(parser: *Parser([]u8, Reader), allocator: *Allocator, src: *Reader) callconv(.Inline) Error!?[]u8 {
            const self = @fieldParentPtr(Self, "parser", parser);
            const buf = try allocator.alloc(u8, self.want.len);
            errdefer allocator.free(buf);

            const read = try src.reader().readAll(buf);
            if (read < self.want.len or !std.mem.eql(u8, buf, self.want)) {
                try src.seekableStream().seekBy(-@intCast(i64, read));
                allocator.free(buf);
                return null;
            }
            return buf;
        }
    };
}

pub fn OneOf(comptime Value: type, comptime Reader: type) type {
    return struct {
        parser: Parser(Value, Reader) = .{
            ._parse = parse,
        },
        parsers: []*Parser(Value, Reader),

        const Self = @This();

        pub fn init(parsers: []*Parser(Value, Reader)) Self {
            return Self{
                .parsers = parsers,
            };
        }

        fn parse(parser: *Parser(Value, Reader), allocator: *Allocator, src: *Reader) callconv(.Inline) Error!?Value {
            const self = @fieldParentPtr(Self, "parser", parser);
            for (self.parsers) | one_of_parser | {
                const result = try one_of_parser.parse(allocator, src);
                if (result != null) {
                    return result;
                }
            }
            return null;
        }
    };
}

const expect = @import("std").testing.expect;
const eql = @import("std").mem.eql;

test "literal" {
    const allocator = std.testing.allocator;

    var reader = std.io.fixedBufferStream("inc a");
    const want: []const u8 = "inc";

    var literal = Literal(@TypeOf(reader)).init(want);
    const p = &literal.parser;

    var result = try p.parse(allocator, &reader);
    expect(eql(u8, want, result.?));
    if (result) |r| {
        allocator.free(r);
    }

    var result2 = try p.parse(allocator, &reader);
    expect(result2 == null);
}

test "oneof" {
    const allocator = std.testing.allocator;

    var reader1 = std.io.fixedBufferStream("dec a");
    var reader2 = std.io.fixedBufferStream("inc a");
    var reader3 = std.io.fixedBufferStream("mul a");
    var one_of = OneOf([]u8, @TypeOf(reader1)).init(&.{
        &Literal(@TypeOf(reader1)).init("dec").parser,
        &Literal(@TypeOf(reader1)).init("inc").parser,
    });

    const p = &one_of.parser;

    var result = try p.parse(allocator, &reader1);
    expect(eql(u8, "dec", result.?));
    if (result) |r| {
        allocator.free(r);
    }

    result = try p.parse(allocator, &reader2);
    expect(eql(u8, "inc", result.?));
    if (result) |r| {
        allocator.free(r);
    }

    result = try p.parse(allocator, &reader3);
    expect(result == null);
}