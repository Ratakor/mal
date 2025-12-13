const std = @import("std");
const types = @import("types.zig");

pub fn print(data: types.Type, writer: anytype) !void {
    switch (data) {
        .int => try writer.print("{d}", .{data.int}),
        .float => try writer.print("{d}", .{data.float}),
        .bool => try writer.print("{}", .{data.bool}),
        .nil => try writer.writeAll("nil"),
        .symbol => try writer.writeAll(data.symbol),
        .string => {
            try writer.writeAll("\"");
            var i: usize = 0;
            while (i < data.string.len) : (i += 1){
                const c = data.string[i];
                if (c == '\\') {
                    if (i + 1 < data.string.len) {
                        const next = data.string[i + 1];
                        switch (next) {
                            '\\' => try writer.writeByte('\\'),
                            '"' => try writer.writeByte('\"'),
                            'n' => try writer.writeByte('\n'),
                            't' => try writer.writeByte('\t'),
                            // 'r' => try writer.writeByte('\r'),
                            else => return error.InvalidEscapeSequence,
                        }
                        i += 1;
                    } else {
                        return error.UnexpectedEndOfString;
                    }
                } else {
                    // switch (c) {
                    //     '\\' => try writer.writeAll("\\\\"),
                    //     '"' => try writer.writeAll("\\\""),
                    //     '\n' => try writer.writeAll("\\n"),
                    //     '\r' => try writer.writeAll("\\r"),
                    //     '\t' => try writer.writeAll("\\t"),
                    //     else => try writer.writeByte(u8),
                    // }
                    try writer.writeByte(c);
                }
            }
            try writer.writeAll("\"");
        },
        .list => {
            try writer.writeAll("(");
            for (data.list.items, 0..) |item, i| {
                if (i != 0) try writer.writeAll(" ");
                try print(item, writer);
            }
            try writer.writeAll(")");
        },
    }
}
