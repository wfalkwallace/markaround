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

   ArrayList<Integer> notes2 = new ArrayList<Integer>();
  notes.add(440.0); 
   notes.add(250.0); 
   notes.add(69.5); 


   new CreateChord(notes);

   
   public CreateChord(ArrayList<Notes> jnotes){
    Score theScore = new Score(); 
    CPhrase chordPhrase1 = new CPhrase();
    Part p1 = new Part("piano", PIANO, 0);
    CPhrase chordPhrase2 = new CPhrase();
    Part p2 = new Part("piano", PIANO, 1);
    for(note: jnotes){
      Note n = new Note (note, 0.5, 0.5); 
      chordPhrase1.addNote(n); 
    }
    p.addPhrase(chordPhrase1); 

    for(note: notes2){
      Note n = new Note (note, 0.5, 0.5); 
      chordPhrase2.addNote(n); 
    }
    p2.addPhrase(chordPhrase2); 

    score.add(p1); 
    score.add(p2); 
    
    Write.midi(theScore, "midi/PartCreate.mid"); 
  }
}