from abc import abstractmethod, ABC
from typing import Optional, Set, Iterable, Tuple

'''This is a python implementation of the Huffman encoding from SICP section 2.3.4.
   Nodes in the huffman tree implement the Node interface.
   Once the tree is constructed, a character's encoding will be computed
   using the `get_character_suffix` method, called from the root of the tree'''

class Node(ABC):
    @abstractmethod
    def get_character_suffix(self, character:str) -> Optional[str]:
        '''Given a character, this method returns a suffix of the encoding
        corresponding to the path to reach the leaf that contains the character.

        When called on the root of the Huffman tree, this will return
        the full encoding of the character.
        
        returns `None` if the character is not in the subtree.'''
        raise NotImplementedError

    @abstractmethod
    def decode_character(self, string:str)->Tuple[str, str]:
        '''given a string of binary digits, returns the character that correspond to it
        and the remainder of the string'''
        raise NotImplementedError

    @abstractmethod
    def get_frequency(self)->int:
        '''Returns the frequency of the node as specified by the Huffman algorithm'''
        raise NotImplementedError
    
class IntermediateNode(Node):
    def __init__(self, left_child:Node, right_child:Node):
        self._left_child = left_child
        self._right_child = right_child

    def get_character_suffix(self, character: str) -> Optional[str]:
        if (left_suffix := self._left_child.get_character_suffix(character)) is not None:
            return '0'+left_suffix
        if (right_suffix := self._right_child.get_character_suffix(character)) is not None:
            return '1'+right_suffix
        return None

    def get_frequency(self) -> int:
        return self._left_child.get_frequency()+self._right_child.get_frequency()

    def decode_character(self, string: str) -> Tuple[str, str]:
        if string == '':
            raise ValueError("String is not valid")
        if string[0] == '0': #branch left
            return self._left_child.decode_character(string[1:])
        else:
            return self._right_child.decode_character(string[1:])

class Leaf(Node):
    def __init__(self, character:str, frequency:int):
        self._character = character
        self._frequency = frequency

    def get_character_suffix(self, character: str)-> Optional[str]:
        return '' if character == self._character else None

    def get_frequency(self) -> int:
        return self._frequency
    
    def decode_character(self, string: str) -> Tuple[str, str]:
        return self._character, string

def build_tree(nodes:Iterable[Node]) -> Node:
    sorted_nodes = sorted(list(nodes), key=lambda node: node.get_frequency())
    if len(sorted_nodes) == 1:
        return sorted_nodes[0]
    node1, node2 = sorted_nodes[0:2]
    new_node = IntermediateNode(node1, node2)
    return build_tree([*sorted_nodes[2:], new_node])

def encode_string(string:str, tree:Node):
    encode_character = lambda character: tree.get_character_suffix(character)
    result = ''
    for character in string:
        encoding = encode_character(character)
        if encoding is None:
            raise ValueError(f"There has been a problem decoding {character}")
        result += encoding
    return result

def decode_string(string:str, tree:Node):
    decode_character = lambda character: tree.decode_character(character)
    result = ''
    while string:
        character, remainder = decode_character(string)
        result += character
        string = remainder
    return result
        

def main():
    alphabet = tuple(zip('abcdefgh', (8, 3, 1, 1, 1, 1, 1, 1)))
    string = "abbcde"
    huffman_tree:Node = build_tree({Leaf(character, frequency) for character, frequency in alphabet})
    print("Encoding: " +
          ", ".join((f"{character}:{encode_string(character, huffman_tree)}"
                    for character, _ in alphabet))+".")
    encoding = encode_string(string, huffman_tree)
    print(f"encoding of `{string}` is `{encoding}`")
    assert huffman_tree.decode_character(huffman_tree.get_character_suffix('a')) == ('a', '')
    print(f"decoding `{encoding}` back gives `{decode_string(encoding, huffman_tree)}`")

if __name__ == '__main__':
    main()
