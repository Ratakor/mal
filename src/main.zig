const std = @import("std");
const Reader = @import("Reader.zig");
const printer = @import("printer.zig");
const types = @import("types.zig");

fn read(allocator: std.mem.Allocator, str: []const u8) !types.Type {
    var reader = Reader.init(str);
    return reader.readForm(allocator);
}

fn eval(ast: types.Type, env: []const u8) types.Type {
    _ = env;
    return ast;
}

fn print(allocator: std.mem.Allocator, exp: types.Type) ![]const u8 {
    var builder = std.ArrayList(u8).init(allocator);
    errdefer builder.deinit();
    try printer.print(exp, builder.writer());
    return builder.toOwnedSlice();
}

fn rep(allocator: std.mem.Allocator, str: []const u8) ![]const u8 {
    return print(allocator, eval(try read(allocator, str), ""));
}

// const ReadLine = struct {
//     arena: std.heap.ArenaAllocator,
//     history: History,
//     current: History.Node,

//     const History = std.DoublyLinkedList([]const u8);

//     pub fn init(child_allocator: std.mem.Allocator) ReadLine {
//         return .{
//             .arena = std.heap.ArenaAllocator.init(child_allocator),
//             .history = .{},
//             .current = undefined,
//         };
//     }

//     pub fn deinit(self: *ReadLine) void {
//         self.arena.deinit();
//     }

//     pub fn readline(self: *ReadLine, prompt: []const u8) !?[]const u8 {
//         const stdout = std.io.getStdOut().writer();
//         const stdin = std.io.getStdIn().reader();
//         try stdout.writeAll(prompt);
//         if (try stdin.readUntilDelimiterOrEofAlloc(self.arena.allocator(), '\n', 4096)) |line| {
//             self.history.append(line);
//             self.current = self.history.last();
//             return line;
//         } else {
//             return null;
//         }
//     }
// };

// TODO: add line editing and command history
fn readline(allocator: std.mem.Allocator, prompt: []const u8) !?[]const u8 {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();
    try stdout.writeAll(prompt);
    return stdin.readUntilDelimiterOrEofAlloc(allocator, '\n', 4096);
}

pub fn main() !void {
    var gpa: std.heap.GeneralPurposeAllocator(.{}) = .{};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();
    const stdout = std.io.getStdOut().writer();

    while (true) {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();

        if (try readline(allocator, "user>")) |line| {
            defer allocator.free(line);
            try stdout.print("{s}\n", .{try rep(arena.allocator(), line)});
        } else {
            try stdout.writeAll("\n");
            break;
        }
    }
}
