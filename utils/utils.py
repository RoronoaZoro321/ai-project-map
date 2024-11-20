# utils/utils.py


def format_time(seconds):
    """
    Converts time in seconds to MM:SS format.

    Args:
        seconds (float): Time in seconds.

    Returns:
        str: Time formatted as MM:SS.
    """
    minutes = int(seconds) // 60
    secs = int(seconds) % 60
    return f"{minutes:02d}:{secs:02d}"
