const std = @import("std");

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

    // for (self.buffer[self.index..], self.index..) |c, i| {
    //     switch (c) {
    //         ' ', ',' => self.index += 1,
    //         '~' => {
    //             if (i + 1 < self.buffer.len and self.buffer[i + 1] == '@') {
    //                 return self.buffer[i..i + 2];
    //             } else {
    //                 return self.buffer[i..i + 1];
    //             }
    //         },
    //         '[', ']', '{', '}', '(', ')', '`', '^', '@' => return self.buffer[i..i + 1],
    //         '\\' => {
    //             if (i + 1 < self.buffer.len and self.buffer[i + 1] == '"') {
    //                 return self.buffer[i..i + 2];
    //             } else {
    //                 return self.buffer[i..i + 1];
    //             }
    //         },
    //         '\"' => {
    //             var end = i + 1;
    //             while (true) {
    //                 end += std.mem.indexOfScalar(u8, self.buffer[end..], '\"') orelse return null;//error.UnmatchedQuote;
    //                 if (self.buffer[end - 1] != '\\') {
    //                     break;
    //                 }
    //             }
    //             return self.buffer[i..end + 1];
    //         },
    //         ';' => return self.buffer[i..],
    //         else => {
    //             const end = i + 1 + (std.mem.indexOfAny(u8, self.buffer[i + 1..], " []{}('\"`,;)") orelse return self.buffer[i..]);
    //             return self.buffer[i..end];
    //         },
    //     }
    // }
    // unreachable;
}

// pub fn peek(self: *Self) ?[]const T {
//     // move to beginning of token
//     while (self.index < self.buffer.len and self.isDelimiter(self.index)) : (self.index += switch (delimiter_type) {
//         .sequence => self.delimiter.len,
//         .any, .scalar => 1,
//     }) {}
//     const start = self.index;
//     if (start == self.buffer.len) {
//         return null;
//     }

//     // move to end of token
//     var end = start;
//     while (end < self.buffer.len and !self.isDelimiter(end)) : (end += 1) {}

//     return self.buffer[start..end];
// }
