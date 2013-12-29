import java.util.ArrayList;
import jm.JMC;
import jm.music.data.*;
import jm.music.tools.*;
import jm.util.*;

/**
* This class turns a series of integers into notes. 
* @author Hila Gutfreund
*/

public class CreateChord implements JMC {
  
  public static void main(String[] args) {
  ArrayList<Integer> notes = new ArrayList<Integer>();
   notes.add(440.0); 
   notes.add(650.0); 
   notes.add(69.0); 

   new CreateChord(notes);

   
   public CreateChord(ArrayList<Notes> jnotes){
    CPhrase chordPhrase = new CPhrase();
    Part p = new Part();
    for(note: jnotes){
      Note n = new Note (note, 0.5, 0.5); 
      chordPhrase.addNote(n); 
    }
    
    
    p.addPhrase(notePhrase); 
    
    Write.midi(p, "midi/creatChord.mid"); 
  }
}