
import jm.JMC;
import jm.music.data.*;
import jm.music.tools.*;
import jm.util.*;

/**
* This class turns a series of integers into notes. 
* @author Hila Gutfreund
*/

public class createNotes implements JMC {
  
  public static void main(String[] args) {
   int[] notes = {30, 250, 54}; 
   new createNotes(notes);
    }
    
  public createNotes(int[] notes){
    Phrase notePhrase = new Phrase();
    for(int note:notes){
      if((note >= 0) && (note <= 127)){
        Note n = new Note(note,1.0);
        notePhrase.addNote(n); 
        
      }else if(note > 127){
        Note n = new Note ((double)note, 0.5); 
        notePhrase.addNote(n); 
      }else{
        Note n = new Note ((double)note, 0.5); 
        notePhrase.addNote(n); 
      }
    }
    Write.midi(notePhrase, "midi/createNotes.mid"); 
  }
}