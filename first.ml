open Music


module B = struct
    let lowest_string = 1
    let second_string = lowest_string + 1
    let third_string = second_string + 1
    let main_start = 4
    let bis_start = 7
    let silence = create_measure [create_rest `Whole]

    module F = struct
      let first = create_measure
                    [create_string_quarter third_string main_start;
                     create_string_eighth ~dot:true third_string main_start;
                     create_string_sixteenth third_string (main_start + 1);
                     create_string_eighth ~dot:true third_string main_start;
                     create_string_sixteenth second_string (main_start + 3);
                     create_string_eighth ~dot:true second_string (main_start + 1);
                     create_string_sixteenth second_string (main_start + 1)]

      let first_bis = create_measure
                        [create_string_quarter third_string bis_start;
                         create_string_eighth ~dot:true third_string bis_start;
                         create_string_sixteenth third_string (bis_start + 2);
                         create_string_eighth ~dot:true third_string bis_start;
                         create_string_sixteenth second_string (bis_start + 3);
                         create_string_eighth ~dot:true second_string (bis_start + 2);
                         create_string_sixteenth second_string (bis_start + 2)]

      let second = create_measure [create_string_eighth ~dot:true second_string (main_start + 3);
                                   create_string_sixteenth second_string (main_start + 3);
                                   create_string_eighth ~dot:true second_string (main_start + 1);
                                   create_string_sixteenth second_string (main_start + 3);
                                   create_string_quarter third_string main_start;
                                   create_string_quarter second_string (main_start + 3)]

      let second_bis = create_measure [create_string_eighth ~dot:true second_string (bis_start + 3);
                                       create_string_sixteenth second_string (bis_start + 3);
                                       create_string_eighth ~dot:true second_string (bis_start + 2);
                                       create_string_sixteenth second_string (bis_start + 3);
                                       create_string_quarter third_string bis_start;
                                       create_string_quarter second_string (bis_start + 3)]

      let third = create_measure [create_string_quarter third_string main_start;
                                  create_string_eighth ~dot:true third_string (main_start + 3);
                                  create_string_sixteenth third_string (main_start + 1);
                                  create_string_eighth ~dot:true third_string main_start;
                                  create_string_sixteenth second_string (main_start + 3);
                                  create_string_eighth ~dot:true second_string (main_start + 1);
                                  create_string_sixteenth lowest_string (main_start + 1)]

      let third_bis = create_measure [create_string_quarter third_string bis_start;
                                      create_string_eighth ~dot:true third_string (bis_start + 4);
                                      create_string_sixteenth third_string (bis_start + 2);
                                      create_string_eighth ~dot:true third_string bis_start;
                                      create_string_sixteenth second_string (bis_start + 3);
                                      create_string_eighth ~dot:true second_string (bis_start + 2);
                                      create_string_sixteenth lowest_string (bis_start + 2)]

      let fourth = create_measure [create_string_eighth ~dot:true second_string (main_start + 1);
                                   create_string_sixteenth second_string (main_start + 3);
                                   create_string_eighth ~dot:true third_string main_start;
                                   create_string_sixteenth third_string (main_start + 1);
                                   create_string_quarter second_string (main_start + 3);
                                   create_string_quarter second_string (main_start + 1)]

      let fourth_bis = create_measure [create_string_eighth ~dot:true second_string (bis_start + 2);
                                       create_string_sixteenth second_string (bis_start + 3);
                                       create_string_eighth ~dot:true third_string bis_start;
                                       create_string_sixteenth third_string (bis_start + 2);
                                       create_string_quarter second_string (bis_start + 3);
                                       create_string_quarter second_string (bis_start + 2)]
      let t = [first; second; third; fourth]
      let t_bis = [first_bis; second_bis; third_bis; fourth_bis]
      let t_silence = [silence; silence; silence; silence]

      end
    module S = struct
      let first = create_measure [create_string_sixteenth third_string main_start;
                                  create_string_eighth ~dot:true second_string (main_start + 1);
                                  create_string_sixteenth third_string main_start;
                                  create_string_eighth ~dot:true third_string main_start;
                                  create_string_sixteenth third_string main_start;
                                  create_string_eighth ~dot:true ~tied:(Some `Start) third_string (main_start + 1);
                                  create_string_quarter ~tied:(Some `Stop) third_string (main_start + 1)]

      let first_bis = create_measure [create_string_sixteenth third_string bis_start;
                                      create_string_eighth ~dot:true second_string (bis_start + 2);
                                      create_string_sixteenth third_string bis_start;
                                      create_string_eighth ~dot:true third_string bis_start;
                                      create_string_sixteenth third_string bis_start;
                                      create_string_eighth ~dot:true ~tied:(Some `Start) third_string (bis_start + 2);
                                      create_string_quarter ~tied:(Some `Stop) third_string (bis_start + 2)]

      let second = create_measure [create_string_sixteenth third_string (main_start + 1);
                                   create_string_eighth ~dot:true third_string main_start;
                                   create_string_sixteenth third_string (main_start + 1);
                                   create_string_eighth ~dot:true third_string main_start;
                                   create_string_sixteenth second_string (main_start + 3);
                                   create_string_eighth ~dot:true ~tied:(Some `Start) second_string (main_start + 1);
                                   create_string_sixteenth lowest_string (main_start + 1);
                                   create_string_eighth ~dot:true second_string (main_start + 1)]

      let second_bis = create_measure [create_string_sixteenth third_string (bis_start + 2);
                                       create_string_eighth ~dot:true third_string bis_start;
                                       create_string_sixteenth third_string (bis_start + 2);
                                       create_string_eighth ~dot:true third_string bis_start;
                                       create_string_sixteenth second_string (bis_start + 3);
                                       create_string_eighth ~dot:true ~tied:(Some `Start) second_string (bis_start + 2);
                                       create_string_sixteenth lowest_string (bis_start + 2);
                                       create_string_eighth ~dot:true second_string (bis_start + 2)]

      let third = create_measure [create_string_sixteenth second_string (main_start + 3);
                                   create_string_eighth ~dot:true third_string main_start;
                                   create_string_sixteenth third_string (main_start + 1);
                                   create_string_quarter second_string (main_start + 3);
                                   create_string_quarter ~tied:(Some `Start) second_string (main_start + 3);
                                   create_string_eighth ~dot:true ~tied:(Some `Stop) second_string (main_start + 3)]

      let third_bis = create_measure [create_string_sixteenth second_string (bis_start + 3);
                                      create_string_eighth ~dot:true third_string bis_start;
                                      create_string_sixteenth third_string (bis_start + 2);
                                      create_string_quarter second_string (bis_start + 3);
                                      create_string_quarter ~tied:(Some `Start) second_string (bis_start + 3);
                                      create_string_eighth ~dot:true ~tied:(Some `Stop) second_string (bis_start + 3)]

      let last = create_measure [create_string_sixteenth second_string (main_start + 3);
                                 create_string_eighth ~dot:true third_string main_start;
                                 create_string_sixteenth third_string (main_start + 1);
                                 create_string_quarter second_string (main_start + 3);
                                 create_string_quarter ~tied:(Some `Start) second_string (main_start + 1);
                                 create_string_eighth ~dot:true ~tied:(Some `Stop) second_string (main_start + 1)]

      let last_bis = create_measure [create_string_sixteenth second_string (bis_start + 3);
                                     create_string_eighth ~dot:true third_string bis_start;
                                     create_string_sixteenth third_string (bis_start + 2);
                                     create_string_quarter second_string (bis_start + 3);
                                     create_string_quarter ~tied:(Some `Start) second_string (bis_start + 2);
                                     create_string_eighth ~dot:true ~tied:(Some `Stop) second_string (bis_start + 1)]

      let t = [first; second; third; first; second; last]
      let t_bis = [first_bis; second_bis; third_bis; first_bis; second_bis; last_bis]
      let t_silence = [silence; silence; silence; silence; silence; silence]
      end

    let guitar_measures = F.t @ S.t
    let guitar_bis_measures = F.t_bis @ S.t_bis
  end


module OneVoice = struct
    type t = {
        guitar: string_note measures;
      }

    let create_part guitar = {guitar}
  end

module TwoVoices = struct
    type t = {
        guitar: string_note measures;
        guitar_bis: string_note measures;
      }

    let create_part guitar guitar_bis = {guitar; guitar_bis}
  end


module First = struct
    open OneVoice
    let song_to_mxml song =
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar)) in
      let xml = Music_xml.create "Iduzki denean (lehen abotsa)" "gaur" 80 [guitar] in
      Music_xml.to_string xml

    let output_example () =
      let song = OneVoice.create_part B.guitar_measures in
      song_to_mxml song

  end

module Second = struct
    open OneVoice
    let song_to_mxml song =
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar)) in
      let xml = Music_xml.create "Iduzki denean (bigarren abotsa)" "gaur" 80 [guitar] in
      Music_xml.to_string xml

    let output_example () =
      let song = create_part B.guitar_bis_measures in
      song_to_mxml song

  end

module All = struct
    open TwoVoices
    let song_to_mxml song =
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar)) in
      let guitar_bis = Music_xml.create_instrument 2 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar_bis)) in
      let xml = Music_xml.create "Iduzki denean" "gaur" 80 [guitar; guitar_bis] in
      Music_xml.to_string xml

    let output_example () =
      let song = create_part B.guitar_measures B.guitar_bis_measures in
      song_to_mxml song

  end

let () =
  let () = Printexc.record_backtrace true in
  let fd = open_out "./iduzki_denean_lehen_abotsa.xml" in
  let () = Printf.fprintf fd "%s" (First.output_example ()) in
  let () = close_out fd in

  let fd = open_out "./iduzki_denean_bigarren_abotsa.xml" in
  let () = Printf.fprintf fd "%s" (Second.output_example ()) in
  let () = close_out fd in

  let fd = open_out "./iduzki_denean.xml" in
  let () = Printf.fprintf fd "%s" (All.output_example ()) in
  close_out fd
