import jm.JMC;
import jm.music.data.*;
import jm.music.tools.*;
import jm.util.*;

/**
* This class turns a series of pitches into a repeating arpeggio
* @author Andrew Brown
*/

public class Arpeggio1 implements JMC {
	
	public static void main(String[] args) {
		 new Arpeggio1();
    }
    
    public Arpeggio1() {
        int[] pitches = {C4, F4, BF4};
        // turn pitches into a phrase
        Phrase arpPhrase = new Phrase();
        for(int i = 0; i < pitches.length; i++) {
            Note n = new Note(pitches[i], SEMI_QUAVER);
            arpPhrase.addNote(n);
        }
        
        // repeat the arpeggio a few times
        Mod.repeat(arpPhrase, 3);
        Mod.repeat(arpPhrase,2); 
        
        // save it as a file
        Write.midi(arpPhrase, "midi/Arpeggio1.mid");

	}
}