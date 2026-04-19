from dataclasses import dataclass

@dataclass
class Pet:
    name: str
    age: int

dog = Pet("Miso", 4)
