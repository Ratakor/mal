const std = @import("std");
const Reader = @import("Reader.zig");

fn read(str: []const u8) []const u8 {
    return str;
}

fn eval(ast: []const u8, env: []const u8) []const u8 {
    _ = env;
    return ast;
}

fn print(exp: []const u8) []const u8 {
    return exp;
}

fn rep(str: []const u8) []const u8 {
    return print(eval(read(str), ""));
}

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

    // var reader = Reader.init(",,,nil,,,tr)ue");
    // while (reader.next()) |token| {
    //     try stdout.print("{s}\n", .{token});
    // }
    // std.process.exit(0);

    while (true) {
        if (try readline(allocator, "user>")) |line| {
            try stdout.print("{s}\n", .{rep(line)});
        } else {
            try stdout.writeAll("\n");
            break;
        }
    }
}
