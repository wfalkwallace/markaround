import jm.JMC;
import jm.util.*;
import jm.music.data.*;
import jm.util.*;

/**
 * This class uses the jMusic CPhrase (Chord Phrase)
 * @author Hila Gutfreund
 */
public final class TwoParts implements JMC{

  private  Score s = new Score("CPhrase class example");
  private  Part piano = new Part("Piano", 0, 0);
  private Part bassPart = new Part("left hand", 0, 1);
  //private double[] rhythms = new double[] {0.25, 0.5, 1.0, 2.0, 4.0};
  //find out what rythms are! 


  public static void main(String[] args){ 
    new TwoParts();
  }

  public TwoParts() {
    int rootPitch = 60; //set start note to middle C
    for (int i = 0; i < 6; i++) {
      firstPart(rootPitch);
      rootPitch -= 7;
      secondPart(rootPitch);
    }

      //pack the part into a score
      s.addPart(piano);
      s.addPart(bassPart);

      // write the score to a MIDIfile
      Write.midi(s, "midi/TwoParts.mid");
    }


    private void firstPart(int rootPitch){
      // build the chord from the rootPitch 
      int[] pitchArray = new int[4];
      pitchArray[0] = rootPitch;
      pitchArray[1] = rootPitch + 4;
      pitchArray[2] = rootPitch + 7;
      pitchArray[3] = rootPitch + 10;
      CPhrase chord = new CPhrase(); 
      chord.addChord(pitchArray, C); 
      piano.addCPhrase(chord); 
    }
    
    private void secondPart(int rootPitch){
      // build the chord from the rootPitch 
      int[] pitchArray = new int[4];
      pitchArray[0] = rootPitch;
      pitchArray[1] = rootPitch + 4;
      pitchArray[2] = rootPitch + 7;
      pitchArray[3] = rootPitch + 10;
      CPhrase chord = new CPhrase(); 
      chord.addChord(pitchArray, C); 
      bassPart.addCPhrase(chord); 
    }

  }