const std = @import("std");

pub const Type = union(enum) {
    int: i64,
    float: f64,
    string: []const u8,
    symbol: []const u8,
    bool: bool,
    nil: void,
    list: List,
};

pub const List = std.ArrayList(Type);

