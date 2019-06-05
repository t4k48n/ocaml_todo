type information_t =
  { name : string;
    date : string; }

let information_create name year month day =
  { name = name;
    date = Printf.sprintf "%04d-%02d-%02d" year month day; }

let information_to_string = function
  | { name=n; date=d } ->
      Printf.sprintf "[%s] %s" d n

type task_t =
  { info : information_t;
    subtasks : information_t list; }

let task_create info =
  { info = info;
    subtasks = []; }

let task_add_subtask t info = match t with
  | { info=i; subtasks=sts } as t ->
      { t with subtasks=info :: sts }

let task_to_string = function
  | { info=i; subtasks=sts; } ->
      let buf = Buffer.create 1024 in
      Buffer.add_string buf (information_to_string i);
      List.iteri (fun i st -> Buffer.add_string buf "\n    "; Buffer.add_string buf (Printf.sprintf "%3d: " (i + 1)); Buffer.add_string buf (information_to_string st)) sts;
      Buffer.contents buf

type tasklist_t = task_t list

let tasklist_add_task tl t = t :: tl

let tasklist_to_string tl =
  let buf = Buffer.create 1024 in
  let sep = ref "" in
  List.iteri
    (fun i t ->
      Buffer.add_string buf !sep;
      sep := "\n";
      Buffer.add_string buf (Printf.sprintf "%3d: " (i + 1));
      Buffer.add_string buf (task_to_string t))
    tl;
  Buffer.contents buf

let () =
  let tasks = [] in
  let tasks = task_create (information_create "Main Task 1" 2019 1 15) :: tasks in
  let t = task_create (information_create "Main Task 2" 2019 6 5) in
  let t = task_add_subtask t (information_create "Sub Task 1" 2019 6 7) in
  let t = task_add_subtask t (information_create "Sub Task 2" 2019 6 8) in
  let t = task_add_subtask t (information_create "Sub Task 3" 2019 6 9) in
  let tasks = tasklist_add_task tasks t in
  let t = task_create (information_create "Main Task 3" 2019 6 5) in
  let t = task_add_subtask t (information_create "Sub Task 1" 2019 6 7) in
  let t = task_add_subtask t (information_create "Sub Task 2" 2019 6 8) in
  let t = task_add_subtask t (information_create "Sub Task 3" 2019 6 9) in
  let tasks = tasklist_add_task tasks t in
  print_endline (tasklist_to_string tasks)
