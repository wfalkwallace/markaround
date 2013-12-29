import javax.sound.midi.*;

public class midiPLTTest {

public static void main(String[] args) {
	
	midiPLTTest mini = new midiPLTTest();
	
	if (args.length < 2) 
	{
		System.out.println("Don't forget the instrument and note args");
	} 
	else 
	{
		int instrument = Integer.parseInt(args[0]);
		int note = Integer.parseInt(args[1]);
		mini.play(instrument,note);
	}
} // close main

public void play(int instrument, int note) 
{
	try {

	//sequencer is A hardware or software device that plays back a MIDI sequence 
	//is known as a sequencer.  
	Sequencer player = MidiSystem.getSequencer();
	player.open();

	//A Sequence is a data structure containing musical information (often an 
	//entire song or composition) that can be played back by a Sequencer object. 
	//Specifically, the Sequence contains timing information and one or more 
	//tracks
	Sequence seq = new Sequence(Sequence.PPQ, 4);

	//ppq is pulses per quarter note

	//A MIDI track is an independent stream of MIDI events 
	//(time-stamped MIDI data) that can be stored along with other tracks in 
	//a standard MIDI file
	//The timing information and resolution for a track is controlled by and 
	//stored in the sequence containing the track. A given Track is considered 
	//to belong to the particular Sequence that maintains its timing. For this 
	//reason, a new (empty) track is created by calling the Sequence.createTrack()
	//method, rather than by directly invoking a Track constructor.
	Track track = seq.createTrack();

	//MIDI events contain a MIDI message and a corresponding time-stamp expressed 
	//in ticks, and can represent the MIDI event information stored in a MIDI file 
	//or a Sequence object. The duration of a tick is specified by the timing 
	//information contained in the MIDI file or Sequence object.
	MidiEvent event = null;


	//A ShortMessage contains a MIDI message that has at most two data bytes 
	//following its status byte. The types of MIDI message that satisfy this 
	//criterion are channel voice, channel mode, system common, and system 
	//real-time--in other words, everything except system exclusive 
	//and meta-events.

	ShortMessage first = new ShortMessage();
	first.setMessage(192, 1, instrument, 0);
	MidiEvent changeInstrument = new MidiEvent(first, 1);
	track.add(changeInstrument);

	ShortMessage a = new ShortMessage();
	a.setMessage(144, 1, note, 100);
	MidiEvent noteOn = new MidiEvent(a, 1);
	track.add(noteOn);

	ShortMessage b = new ShortMessage();
	b.setMessage(128, 1, note, 100);
	MidiEvent noteOff = new MidiEvent(b, 16);
	track.add(noteOff);

	player.setSequence(seq);
	player.start();
	} catch (Exception ex) {ex.printStackTrace();}
	} // close play

} // close class