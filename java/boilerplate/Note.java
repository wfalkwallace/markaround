/**
 * 
 */

/**
 * @author wgf2104
 *
 */
public class Note {
	
	int pitch, duration, volume, instrument;
	boolean pitch_bend, tremolo, vibrato;
	
	/**
	 * Constructor takes integer/boolean values representing note.
	 * @param pitch
	 * @param duration
	 * @param volume
	 * @param instrument
	 * @param pitch_bend
	 * @param tremolo
	 * @param vibrato
	 */
	public Note(int pitch, int duration, int volume, int instrument, boolean pitch_bend, boolean tremolo, boolean vibrato) {
		this.pitch = pitch;
		this.duration = duration;
		this.volume = volume;
		this.instrument = instrument;
		this.pitch_bend = pitch_bend;
		this.tremolo = tremolo;
		this.vibrato = vibrato;
	}
	
	/**
	 * @return the pitch
	 */
	public int getPitch() {
		return pitch;
	}
	/**
	 * @param pitch the pitch to set
	 */
	public void setPitch(int pitch) {
		this.pitch = pitch;
	}
	/**
	 * @return the duration
	 */
	public int getDuration() {
		return duration;
	}
	/**
	 * @param duration the duration to set
	 */
	public void setDuration(int duration) {
		this.duration = duration;
	}
	/**
	 * @return the volume
	 */
	public int getVolume() {
		return volume;
	}
	/**
	 * @param volume the volume to set
	 */
	public void setVolume(int volume) {
		this.volume = volume;
	}
	/**
	 * @return the instrument
	 */
	public int getInstrument() {
		return instrument;
	}
	/**
	 * @param instrument the instrument to set
	 */
	public void setInstrument(int instrument) {
		this.instrument = instrument;
	}
	/**
	 * @return the pitch_bend
	 */
	public boolean isPitch_bend() {
		return pitch_bend;
	}
	/**
	 * @param pitch_bend the pitch_bend to set
	 */
	public void setPitch_bend(boolean pitch_bend) {
		this.pitch_bend = pitch_bend;
	}
	/**
	 * @return the tremolo
	 */
	public boolean isTremolo() {
		return tremolo;
	}
	/**
	 * @param tremolo the tremolo to set
	 */
	public void setTremolo(boolean tremolo) {
		this.tremolo = tremolo;
	}
	/**
	 * @return the vibrato
	 */
	public boolean isVibrato() {
		return vibrato;
	}
	/**
	 * @param vibrato the vibrato to set
	 */
	public void setVibrato(boolean vibrato) {
		this.vibrato = vibrato;
	}


}