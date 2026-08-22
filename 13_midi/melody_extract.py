from pathlib import Path
import numpy as np
import torch
import librosa
import torchcrepe
import pretty_midi
from scipy.signal import medfilt

# ---------- settings ----------
SR = 16000                    # CREPE expects 16 kHz
HOP_LENGTH = 160              # 10 ms hop at 16kHz
FMIN, FMAX = 80.0, 1200.0     # adjust to your melody range
CONF_TH = 0.35                # confidence threshold
MEDIAN_MS = 90                # smoothing window (ms)
MIN_NOTE_MS = 120             # discard super-short notes
# -----------------------------

def hz_to_midi(hz: float) -> int:
    return int(np.round(69 + 12 * np.log2(hz / 440.0)))

def extract_melody_midi(wav_path: Path, midi_path: Path):
    # Load mono audio at 16k
    audio, _ = librosa.load(wav_path, sr=SR, mono=True)
    audio_t = torch.tensor(audio, dtype=torch.float32)[None, :]  # (1, T)

    with torch.no_grad():
        # pitch: (1, frames), periodicity: (1, frames)
        pitch, periodicity = torchcrepe.predict(
            audio_t,
            SR,
            HOP_LENGTH,
            fmin=FMIN,
            fmax=FMAX,
            model="full",      # "full" is more accurate than "tiny"
            batch_size=512,
            device="cpu",
            return_periodicity=True,
        )

    f0 = pitch[0].cpu().numpy()
    conf = periodicity[0].cpu().numpy()

    # Mask low-confidence frames
    f0[conf < CONF_TH] = 0.0

    # Smooth pitch (median filter)
    win = max(1, int((MEDIAN_MS / 1000.0) * (SR / HOP_LENGTH)))
    if win % 2 == 0:
        win += 1
    f0 = medfilt(f0, kernel_size=win)

    # Convert contour -> note segments
    times = np.arange(len(f0)) * (HOP_LENGTH / SR)

    notes = []
    cur_note = None
    cur_start = None

    def flush(note, start, end):
        if note is None:
            return
        dur_ms = (end - start) * 1000.0
        if dur_ms >= MIN_NOTE_MS:
            notes.append((note, start, end))

    for t, hz in zip(times, f0):
        if hz <= 0:
            if cur_note is not None:
                flush(cur_note, cur_start, t)
                cur_note, cur_start = None, None
            continue

        midi = hz_to_midi(hz)

        if cur_note is None:
            cur_note, cur_start = midi, t
        else:
            # New note if pitch changes enough (>= 1 semitone)
            if abs(midi - cur_note) >= 1:
                flush(cur_note, cur_start, t)
                cur_note, cur_start = midi, t

    # flush tail
    flush(cur_note, cur_start, times[-1] + (HOP_LENGTH / SR))

    # Write MIDI
    pm = pretty_midi.PrettyMIDI()
    inst = pretty_midi.Instrument(program=0)  # Acoustic Grand Piano
    for midi_note, start, end in notes:
        inst.notes.append(pretty_midi.Note(
            velocity=90, pitch=int(midi_note), start=float(start), end=float(end)
        ))
    pm.instruments.append(inst)
    pm.write(str(midi_path))

    print(f"Saved MIDI: {midi_path}")
    print(f"Notes: {len(notes)}")

if __name__ == "__main__":
    # in_wav = "./separated/htdemucs/thronebreaker_clipped/other.wav"
    in_wav = "./thronebreaker_clipped.mp3"
    out_mid = "./out_melody/other_melody_crepe.mid"
    print('hello')
    extract_melody_midi(in_wav, out_mid)
