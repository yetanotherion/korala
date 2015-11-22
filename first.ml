open Music


type parts = {
    guitar: string_note measures;
    guitar_two: string_note measures;
  }

let create_part guitar guitar_two = {guitar; guitar_two}

let append_parts a b =
  {
    guitar = a.guitar @ b.guitar;
    guitar_two = b.guitar_two @ b.guitar_two;
  }

let flatten l =
  match l with
  | [] -> {guitar = [];
           guitar_two = [];}
  | hd :: tl ->
     List.fold_left (fun accum x ->
                     append_parts accum x) hd tl

module B = struct
    let lowest_string = 1
    let second_string = lowest_string + 1
    let third_string = second_string + 1

    module F = struct
      let first = create_measure
                    [create_string_quarter third_string 4;
                     create_string_eighth ~dot:true third_string 4;
                     create_string_sixteenth third_string 5;
                     create_string_eighth ~dot:true third_string 4;
                     create_string_sixteenth second_string 7;
                     create_string_eighth ~dot:true second_string 5;
                     create_string_sixteenth second_string 5]

      let second = create_measure [create_string_eighth ~dot:true second_string 7;
                                   create_string_sixteenth second_string 7;
                                   create_string_eighth ~dot:true second_string 5;
                                   create_string_sixteenth second_string 7;
                                   create_string_quarter third_string 4;
                                   create_string_quarter second_string 7]
      let third = create_measure
                    [create_string_quarter third_string 4;
                     create_string_eighth ~dot:true third_string 7;
                     create_string_sixteenth third_string 5;
                     create_string_eighth ~dot:true third_string 4;
                     create_string_sixteenth second_string 7;
                     create_string_eighth ~dot:true second_string 5;
                     create_string_sixteenth lowest_string 5]

      let fourth = create_measure [create_string_eighth ~dot:true second_string 5;
                                   create_string_sixteenth second_string 7;
                                   create_string_eighth ~dot:true third_string 4;
                                   create_string_sixteenth third_string 5;
                                   create_string_quarter second_string 7;
                                   create_string_quarter second_string 5]
      let t = [first; second; third; fourth]
      end
    module S = struct
      let first = create_measure
                    [create_string_sixteenth third_string 4;
                     create_string_eighth ~dot:true second_string 5;
                     create_string_sixteenth third_string 4;
                     create_string_eighth ~dot:true third_string 4;
                     create_string_sixteenth third_string 4;
                     create_string_eighth ~dot:true ~tied:(Some `Start) third_string 5;
                     create_string_quarter ~tied:(Some `Stop) third_string 5]

      let second = create_measure [create_string_sixteenth third_string 5;
                                   create_string_eighth ~dot:true third_string 4;
                                   create_string_sixteenth third_string 5;
                                   create_string_eighth ~dot:true third_string 4;
                                   create_string_sixteenth second_string 7;
                                   create_string_eighth ~dot:true ~tied:(Some `Start) second_string 5;
                                   create_string_sixteenth lowest_string 5;
                                   create_string_eighth ~dot:true second_string 5]

      let third = create_measure [create_string_sixteenth second_string 7;
                                   create_string_eighth ~dot:true third_string 4;
                                   create_string_sixteenth third_string 5;
                                   create_string_quarter second_string 7;
                                   create_string_quarter ~tied:(Some `Start) second_string 7;
                                   create_string_eighth ~dot:true ~tied:(Some `Stop) second_string 7]

      let last = create_measure [create_string_sixteenth second_string 7;
                                 create_string_eighth ~dot:true third_string 4;
                                 create_string_sixteenth third_string 5;
                                 create_string_quarter second_string 7;
                                 create_string_quarter ~tied:(Some `Start) second_string 5;
                                 create_string_eighth ~dot:true ~tied:(Some `Stop) second_string 5]

      let t = [first; second; third; first; second; last]
      end
    let guitar_measures = F.t @ S.t
    let guitar_two_measures = transpose_measures 4 guitar_measures
    let t = create_part guitar_measures guitar_two_measures
  end

module Example = struct

    let song_to_mxml song =
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar)) in
      let guitar2 = Music_xml.create_instrument 2 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar_two)) in
      let xml = Music_xml.create "Iduzki denean" "gaur" 80 [guitar; guitar2] in
      Music_xml.to_string xml

    let output_example () =
      let song = flatten [B.t] in
      song_to_mxml song

  end

let () =
  let () = Printexc.record_backtrace true in
  let fd = open_out "./iduzki_denean.xml" in
  let () = Printf.fprintf fd "%s" (Example.output_example ()) in
  close_out fd
