import java.util.ArrayList;

/**
 * 
 */

/**
 * @author wgf2104
 *
 */
public class Track {
	
	public ArrayList<Chord> track;
	
	/**
	 * Empty track constructor
	 */
	public Track() {
		track = new ArrayList<Chord>();
	}
	
	/**
	 * Chord elevation Track constructor.
	 * Builds a single-chord track from a chord
	 * @param track
	 */
	public Track(Chord other) {
		track = new ArrayList<Chord>();
		track.add(other);
	}
	
	/**
	 * Track copy-constructor
	 * @param track
	 */
	public Track(Track other) {
		track = new ArrayList<Chord>();
		//copy-constructor so pass-by-value of chord references
		track.addAll(other.getTrack());
	}
	
	/**
	 * Get the actual arraylist
	 * @return
	 */
	public ArrayList<Chord> getTrack() {
		return track;
	}
	
	/**
	 * 
	 * @return
	 */
	public Chord getChord(int index) {
		return track.get(index);
	}
	
	/**
	 * 
	 * @return
	 */
	public int length() {
		return track.size();
	}

	/**
	 * serial add tracks
	 * @param other
	 */
	public Track serialAdd(Track other) {
		//immutable.
		Track tmp = new Track(this); 
		//addAll is arraylist concatenation
		tmp.getTrack().addAll(other.getTrack());;
		return tmp;
	}
	
	/**
	 * add a chord to a track. "elevation" is handled
	 * @param other
	 */
	public Track serialAdd(Chord other) {
		Track tmp = new Track(this); 
		tmp.getTrack().add(other);;
		return tmp;
	}

	/**
	 * parallel add chord to track
	 * @param track
	 */
	public void parallelAdd(Chord other) {
		Track tmp = new Track(this);
		tmp.getChord(0).parallelAdd(other);
	}
	
	/**
	 * parallel add track to track
	 * @param track
	 */
	public void parallelAdd(Track other) {
		Track tmp = new Track(this);
		//go through the length of the shorter track
		for(int i = 0; i < (this.length() < other.length() ? this.length() : other.length()); i++) {
			//par-add the second operand track to the tmp (left operand copy) track element at the same place
			tmp.getChord(i).parallelAdd(other.getChord(i));
		}
	}
}
