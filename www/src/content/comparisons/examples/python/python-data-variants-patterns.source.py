def can_walk(light: str) -> bool:
    match light:
        case "green":
            return True
        case _:
            return False
