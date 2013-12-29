import java.util.*;
import jm.JMC;
import jm.music.data.*;
import jm.util.*;

public class DJ implements JMC{

public static void main(String[] args){
double pitchA;
double volume;
double duration;
pitchA = 440;
volume = 100;
duration = 4;
Note n = new Note((double)pitchA, duration, (int) volume);
CPhrase c =  new CPhrase();
Note [] notes_array = {n};
c.addChord(notes_array);
Part t =  new Part();
t.addCPhrase(c);

Score s =  new Score();
s.addPart(t);
Write.midi(s, "createNotes.mid");
}
}