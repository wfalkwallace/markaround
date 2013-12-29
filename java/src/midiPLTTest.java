import javax.sound.midi.*;

public class midiPLTTest {

public static void main(String[] args) {
midiPLTTest mini = new midiPLTTest();
if (args.length < 2) {
System.out.println("Don't forget the instrument and note args");
} else {

//sound-synthesis algorithm with certain parameter settings usually emulate specific real world instruments. 
  //stored in collection (soundbanks)
  //must first be loaded onto synthesizer and then it must be selected for use on one more channels
int instrument = Integer.parseInt(args[0]);


int note = Integer.parseInt(args[1]);
mini.play(instrument,note);
}
} // close main

//method that plays the note
public void play(int instrument, int note) {
try {

//sequencer is a hardware or software device that plays back a midi
//sequence. 

Sequencer player = MidiSystem.getSequencer();

//
player.open();

//sequence is a data structure containing musical info (song or composition) that 
//can be played back by a sequencer. it contains timing info and one or more tracks. 
//PPQ == the tempo based timing tpe for which the resolution is expressed in pulses (ticks) per quarter note
Sequence seq = new Sequence(Sequence.PPQ, 4);

//an independent stream of midi events that can be stored along with other tracks
// with other tracks in a midi file. a midii file can contina any number of trakcs. 
Track track = seq.createTrack();

//events contain a midi message and a corresponding time-stamp expressed in time ticks and can be represented the midi event info
//stored in a midi file or a sequence object. the duration of a tick is specified by a timing info contained in the midi file or seq obj
MidiEvent event = null;


ShortMessage first = new ShortMessage();
first.setMessage(192, 1, instrument, 0);
MidiEvent changeInstrument = new MidiEvent(first, 1);
track.add(changeInstrument);

//shortmessage baiscally allows you to put put in midi data bytes
ShortMessage a = new ShortMessage();
a.setMessage(144, 1, note, 100); //sets the parameters for message: takes up to two bytes?
//command for note on message
MidiEvent noteOn = new MidiEvent(a, 1);
track.add(noteOn);

ShortMessage b = new ShortMessage();
b.setMessage(128, 1, note, 100);
//command for note off message
MidiEvent noteOff = new MidiEvent(b, 16);
track.add(noteOff);

player.setSequence(seq);
player.start();
} catch (Exception ex) {ex.printStackTrace();}
} // close play

} // close class