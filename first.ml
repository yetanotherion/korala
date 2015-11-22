open Music


type parts = {
    guitar: string_note measures;
  }

let create_part guitar = {guitar}

let append_parts a b =
  {
    guitar = a.guitar @ b.guitar;
  }

let flatten l =
  match l with
  | [] -> {guitar = []}
  | hd :: tl ->
     List.fold_left (fun accum x ->
                     append_parts accum x) hd tl

module B = struct
    module F = struct
      let first = create_measure
                    [create_string_quarter 3 4;
                     create_string_eighth ~dot:true 3 4;
                     create_string_sixteenth 3 5;
                     create_string_eighth ~dot:true 3 4;
                     create_string_sixteenth 2 7;
                     create_string_eighth ~dot:true 2 5;
                     create_string_sixteenth 2 5]

      let second = create_measure [create_string_eighth ~dot:true 2 7;
                                   create_string_sixteenth 2 7;
                                   create_string_eighth ~dot:true 2 5;
                                   create_string_sixteenth 2 7;
                                   create_string_quarter 3 4;
                                   create_string_quarter 2 7]
      let third = create_measure
                    [create_string_quarter 3 4;
                     create_string_eighth ~dot:true 3 7;
                     create_string_sixteenth 3 5;
                     create_string_eighth ~dot:true 3 4;
                     create_string_sixteenth 2 7;
                     create_string_eighth ~dot:true 2 5;
                     create_string_sixteenth 1 5]

      let fourth = create_measure [create_string_eighth ~dot:true 2 5;
                                   create_string_sixteenth 2 7;
                                   create_string_eighth ~dot:true 3 4;
                                   create_string_sixteenth 3 5;
                                   create_string_quarter 2 7;
                                   create_string_quarter 2 5]
      let t = [first; second; third; fourth]
      end
    module S = struct
      let first = create_measure
                    [create_string_sixteenth 3 4;
                     create_string_eighth ~dot:true 2 5;
                     create_string_sixteenth 3 4;
                     create_string_eighth ~dot:true 3 4;
                     create_string_sixteenth 3 4;
                     create_string_eighth ~dot:true ~tied:(Some `Start) 3 5;
                     create_string_quarter ~tied:(Some `Stop) 3 5]

      let second = create_measure [create_string_sixteenth 3 5;
                                   create_string_eighth ~dot:true 3 4;
                                   create_string_sixteenth 3 5;
                                   create_string_eighth ~dot:true 3 4;
                                   create_string_sixteenth 2 7;
                                   create_string_eighth ~dot:true ~tied:(Some `Start) 2 5;
                                   create_string_sixteenth 1 5;
                                   create_string_eighth ~dot:true 2 5]

      let third = create_measure [create_string_sixteenth 2 7;
                                   create_string_eighth ~dot:true 3 4;
                                   create_string_sixteenth 3 5;
                                   create_string_quarter 2 7;
                                   create_string_quarter ~tied:(Some `Start) 2 7;
                                   create_string_eighth ~dot:true ~tied:(Some `Stop) 2 7]

      let last = create_measure [create_string_sixteenth 2 7;
                                 create_string_eighth ~dot:true 3 4;
                                 create_string_sixteenth 3 5;
                                 create_string_quarter 2 7;
                                 create_string_quarter ~tied:(Some `Start) 2 5;
                                 create_string_eighth ~dot:true ~tied:(Some `Stop) 2 5]

      let t = [first; second; third; first; second; last]
      end
    let t = create_part (F.t @ S.t)
  end

module Example = struct

    let song_to_mxml song =
      let guitar = Music_xml.create_instrument 1 Music_xml.MidiInstruments.std_guitar (`String (std_guitar, song.guitar)) in
      let xml = Music_xml.create "Iduzki denean" "gaur" 80 [guitar] in
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
