const std = @import("std");
const mem = std.mem;
const math = std.math;
const ArrayList = std.ArrayList;
const parseInt = std.fmt.parseInt;
const Io = std.Io;

const Box = struct {
 length: u32,
 width: u32,
 height: u32
};

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const file = try Io.Dir.cwd().openFile(io, "input2.txt", .{});
    var data: [10240]u8 = undefined;
    const len = try file.readPositionalAll(io, &data, 0);
    std.debug.print("data: {s}\n", .{data[0..len]});
    var boxes = parse(init.gpa, data[0..len]);
    std.debug.print("box len: {d}\n", .{boxes.items.len});
    defer boxes.deinit(init.gpa);
    var res: u64 = 0;
    for(boxes.items) |box| {
        res += box.length;
    }
    std.debug.print("box res: {d}\n", .{res});
    var res2: u64 = 0;
    for(boxes.items) |box| {
        var sorted = [_]u32 { box.height , box.width  ,  box.length };
        std.mem.sort(u32,
            &sorted,  {}, comptime std.sort.asc(u32)
            );
        res2 += box.height * box.width * box.length + sorted[0] * 2 + sorted[1] * 2;
    }
    std.debug.print("box res2: {d}\n", .{res2});
}


fn parse(gpa: mem.Allocator, buf: []const u8) ArrayList(Box){
  var res: ArrayList(Box) = .empty;
  //res.append(gpa,) catch {};
  var it = std.mem.tokenizeAny(u8, buf, "\n");
    while (it.next()) |line| {
      std.debug.print("line: {s}\n", .{line});
      var items = std.mem.tokenizeAny(u8, line, "x");
      var list: std.ArrayList(u32) = .empty;
      defer list.deinit(gpa);
      while (items.next()) |item| {
          const n = parseInt(u32, item, 10) catch 999;
          list.append(gpa, n) catch {};
        }
      const b = Box{.length = list.items[0], .width = list.items[1], .height = list.items[2]};
      res.append(gpa, b) catch {};
    }

  return res;
}

pub fn count_floors(comptime inp:  []const u8) i32 {
    var res: i32 = 0;
    for (inp) |v| {
       if(v=='(') res +=1
       else res-=1;
    }
    return res;
}

pub fn find_base_pos(inp:  []const u8) i32 {
    var pos: i32 = 0;
    var res: i32 = 0;
    for (inp) |v| {
       pos += 1;
       if(v=='(') res +=1
       else res-=1;
    if(res<0) return pos;
    }
    return 0;
}

test "empty" {
    try std.testing.expectEqual(@as(i32, 0), count_floors(""));
}
test "r_2" {
    try std.testing.expectEqual(@as(i32, 2), count_floors("(("));
}


