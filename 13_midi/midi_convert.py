from music21 import converter
import os

print(os.getcwd())
print(os.listdir())

score = converter.parse("13_midi/out/thronebreaker_basic_pitch.mid")
score.show('midi')

from pydub import AudioSegment

# Load MP3
audio = AudioSegment.from_mp3("13_midi/the_end.mp3")

# Define slice (mm:ss → ms)
start_ms = (9 * 60 + 1) * 1000   # 2:50
end_ms   = (9 * 60 + 15) * 1000    # 3:06

# Slice audio
clip = audio[start_ms:end_ms]

# Export
clip.export("13_midi/the_end_clipped.mp3", format="mp3")


from pydub import AudioSegment

wav_files = [
    "13_midi/other.wav",
    "13_midi/drums.wav",
]

for wav in wav_files:
    audio = AudioSegment.from_wav(wav)
    mp3_name = wav.replace(".wav", ".mp3")
    audio.export(mp3_name, format="mp3", bitrate="320k")
