const std = @import("std");
const types = @import("types.zig");

const Reader = @This();

buffer: []const u8,
index: usize,

pub fn init(str: []const u8) Reader {
    return .{
        .buffer = str,
        .index = 0,
    };
}

pub fn next(self: *Reader) ?[]const u8 {
    const token = self.peek() orelse return null;
    self.index += token.len;
    return token;
}

pub fn peek(self: *Reader) ?[]const u8 {
    self.index += std.mem.indexOfNone(u8, self.buffer[self.index..], " ,") orelse return null;
    const i = self.index;
    switch (self.buffer[i]) {
        '~' => {
            if (i + 1 < self.buffer.len and self.buffer[i + 1] == '@') {
                return self.buffer[i..i + 2];
            } else {
                return self.buffer[i..i + 1];
            }
        },
        '[', ']', '{', '}', '(', ')', '`', '^', '@' => return self.buffer[i..i + 1],
        '\\' => {
            if (i + 1 < self.buffer.len and self.buffer[i + 1] == '"') {
                return self.buffer[i..i + 2];
            } else {
                return self.buffer[i..i + 1];
            }
        },
        '\"' => {
            var end = i + 1;
            while (true) {
                end += std.mem.indexOfScalar(u8, self.buffer[end..], '\"') orelse return null;//error.UnmatchedQuote;
                if (self.buffer[end - 1] != '\\') {
                    break;
                }
            }
            return self.buffer[i..end + 1];
        },
        ';' => return self.buffer[i..],
        else => {
            const end = i + 1 + (std.mem.indexOfAny(u8, self.buffer[i + 1..], " []{}('\"`,;)") orelse return self.buffer[i..]);
            return self.buffer[i..end];
        },
    }
}

const Error = error{NoToken, UnmatchedParentheses} || std.mem.Allocator.Error;

pub fn readForm(self: *Reader, allocator: std.mem.Allocator) Error!types.Type {
    const token = self.next() orelse return error.NoToken;
    switch (token[0]) {
        '(' => return self.readList(allocator),
        // '[' => return self.readVector(allocator),
        // ':' => return self.readKeyword(allocator),
        // '{' => return self.readMap(allocator),
        '\"' => return .{ .string = token[1..token.len - 1] }, // TODO: unescape (the stuff done in printer ....)
        else => {},
    }

    if (std.fmt.parseInt(i64, token, 10)) |n| {
        return .{ .int = n };
    } else |_| if (std.fmt.parseFloat(f64, token)) |f| {
        return .{ .float = f };
    } else |_| if (std.mem.eql(u8, token, "nil")) {
        return .{ .nil = {} };
    } else if (std.mem.eql(u8, token, "true")) {
        return .{ .bool = true };
    } else if (std.mem.eql(u8, token, "false")) {
        return .{ .bool = false };
    } else {
        return self.readAtom(token);
    }
}

fn readList(self: *Reader, allocator: std.mem.Allocator) Error!types.Type {
    var list = types.List.init(allocator);
    errdefer list.deinit();
    while (self.peek()) |token| {
        if (token[0] == ')') {
            _ = self.next();
            return .{ .list = list };
        }
        try list.append(try self.readForm(allocator));
    }
    return error.UnmatchedParentheses;
}

fn readAtom(self: *Reader, token: []const u8) types.Type {
    _ = self;
    return .{ .symbol = token };
}
