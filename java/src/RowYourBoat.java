import jm.JMC;
import jm.music.data.*;
import jm.util.*;
import jm.music.tools.*;
/**
 * Plays a melody as a round in three parts
 * @author Andrew Sorensen and Andrew Brown with comments for understanding by Hila 
 * 
 * 
 * Took this so that we could understand looping. 
 */
 
public final class RowYourBoat implements JMC{

  public static void main(String[] args){
    //Create the data objects we want to use
    Score score = new Score("Row Your Boat");
    //Parts can have a name, instrument, and channel.
    Part flute = new Part("Flute", FLUTE, 0);
    Part trumpet = new Part("Trumpet", TRUMPET, 1);
    Part clarinet = new Part("Clarinet", CLARINET, 2);
    
    //Lets write the music in a convenient way.

    //these are the actual notes. This is not how we want to write ours
    //but for the purpose of learning loops its ok. 
    int[] pitchArray = {C4,C4,C4,D4,E4,E4,D4,E4,F4,G4,C5,C5,C5,G4,G4,G4,E4,E4,E4,
                C4,C4,C4,G4,F4,E4,D4,C4};
    
    //this is rythm. this is quarter notes QT, whole notes C, etc. 
    double[] rhythmArray = {C, C,CT,QT,C,CT,QT,CT, QT, M, QT, QT, QT, QT, QT,
              QT, QT, QT, QT, QT, QT, QT, CT, QT, CT, QT, M};
    //add the notes to a phrase
    Phrase phrase1 = new Phrase(0.0);
    phrase1.addNoteList(pitchArray, rhythmArray);
    
    //Make two new phrases and change start times to make a round
    Phrase phrase2 = phrase1.copy();
    phrase2.setStartTime(4.0);
    Phrase phrase3 = phrase1.copy();
    phrase3.setStartTime(8.0);
    
    //Play different parts in different octaves
    // mod == A utility class that handles the modification of the basic jMusic types.
    Mod.transpose(phrase1, 12);
    Mod.transpose(phrase3, -12);
    
    //loop phrases once
    //Makes the CPhrase n times as long by repeating.
    Mod.repeat(phrase1, 1);
    Mod.repeat(phrase2, 1);
    Mod.repeat(phrase3, 1);
    
    //add phrases to the parts
    flute.addPhrase(phrase1);   
    trumpet.addPhrase(phrase2);
    clarinet.addPhrase(phrase3);
    
    //add parts to the score
    score.addPart(flute);
    score.addPart(trumpet); 
    score.addPart(clarinet);    
        
    //OK now we do a SMF write 
    Write.midi(score, "midi/rowboat.mid");
  }
}