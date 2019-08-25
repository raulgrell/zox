const std = @import("std");
const debug = std.debug;
const assert = debug.assert;
const testing = std.testing;

pub fn SinglyLinkedList(comptime T: type) type {
    return struct {
        const Self = @This();

        /// Node inside the linked list wrapping the actual data.
        pub const Node = struct {
            next: ?*Node,
            data: T,

            pub fn init(data: T) Node {
                return Node{
                    .next = null,
                    .data = data,
                };
            }
        };

        first: ?*Node,

        /// Initialize a linked list.
        ///
        /// Returns:
        ///     An empty linked list.
        pub fn init() Self {
            return Self{
                .first = null,
            };
        }

        /// Insert a new node at the head.
        ///
        /// Arguments:
        ///     new_node: Pointer to the new node to insert.
        pub fn prepend(list: *Self, new_node: *Node) void {
            new_node.next = list.first;
            list.first = new_node;
        }

        /// Remove a node from the list.
        ///
        /// Arguments:
        ///     node: Pointer to the node to be removed.
        pub fn remove(list: *Self, node: *Node) void {
            if (list.first == node) {
                list.first = node.next;
            } else {
                var current_elm = list.first.?;
                while (current_elm.next != node) : (current_elm = current_elm.next.?) {}
                current_elm.next = node.next;
            }
        }
    };
}

test "basic SinglyLinkedList test" {
    var list = SinglyLinkedList(u32).init();
    var one = SinglyLinkedList(u32).Node.init(1);
    list.prepend(&one);
    var first = list.remove(list.first.?);
}