from abc import abstractmethod, ABC
from typing import Optional, Set, Iterable

'''This is a python implementation of the Huffman encoding from SICP section 2.3.4.
   Nodes in the huffman tree implement the Node interface.
   Once the tree is constructed, a letter's encoding will be computed
   using the `get_letter_suffix` method called on the root of the tree'''

class Node(ABC):
    @abstractmethod
    def get_letter_suffix(self, letter:str) -> Optional[str]:
        '''Given a letter, this method returns a suffix of the encoding
        corresponding to the path to reach the leaf that contains the letter.

        When called on the root of the Huffman tree, this will return
        the full encoding of the letter.
        
        return `None` if the letter is not in the subtree.'''
        raise NotImplementedError

    @abstractmethod
    def get_frequency(self)->int:
        '''Returns the frequency of the node as specified by the Huffman algorithm'''
        raise NotImplementedError
    
class MiddleNode(Node):
    def __init__(self, left_child:Node, right_child:Node):
        self._left_child = left_child
        self._right_child = right_child

    def get_letter_suffix(self, letter: str) -> Optional[str]:
        if left_suffix := self._left_child.get_letter_suffix(letter) is not None:
            return '0'+left_suffix
        if right_suffix := self._right_child.get_letter_suffix(letter) is not None:
            return '1'+right_suffix
        #otherwise None

    def get_frequency(self) -> int:
        return self._left_child.get_frequency()+self._right_child.get_frequency()

class Leaf(Node):
    def __init__(self, letter:str, frequency:int):
        self._letter = letter
        self._frequency = frequency

    def get_letter_suffix(self, letter: str)-> Optional[str]:
        return '' if letter == self._letter else None

    def get_frequency(self) -> int:
        return self._frequency

def build_tree(nodes:Iterable[Node]) -> Node:
    sorted_nodes = sorted(list(nodes), key=lambda node: node.get_frequency())
    if len(sorted_nodes) == 1:
        return sorted_nodes[0]
    node1, node2 = sorted_nodes[0:2]
    new_node = MiddleNode(node1, node2)
    return build_tree([*sorted_nodes[2:], new_node])

def main():
    alphabet = zip('abcdefgh', [8, 3, 1, 1, 1, 1, 1, 1])
    node_abc:Node = build_tree({Leaf(letter, frequency) for letter, frequency in alphabet})
    encode = lambda letter: node_abc.get_letter_suffix(letter)
    for letter in 'abcdefgh':
        print(f"{letter} encoding is {encode(letter)}")

if __name__ == '__main__':
    main()
