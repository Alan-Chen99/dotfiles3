from collections import deque
from pathlib import PurePosixPath

import pygit2
import regexfactory as r
from pygit2 import Blob, Commit, Oid, Tree

repo_path = "/home/alan/dotfiles_tmp/.git"
repo = pygit2.Repository(repo_path)


head_ref = repo.references.get(f"refs/heads/master")
assert head_ref is not None
head = head_ref.peel(None)

relavent_commits: list[Commit] = []


def explore(x: Commit):
    if x in relavent_commits:
        return
    relavent_commits.append(x)
    for y in x.parents:
        explore(y)


explore(head)

commits = sorted(reversed(relavent_commits), key=lambda x: x.commit_time)
processed: dict[Oid, Oid] = {}

# for x in commits:
#     print(x.short_id, x.commit_time, x.message)

letter = r.Range("a", "z") | r.Range("A", "Z") | "-"
letter_all = letter | r.DIGIT
rx = letter_all[:] + (letter[1:] + r.DIGIT[1:])[4:] + letter_all[:]
rx_ = rx.compile()


def process_line(line: str):
    if (match := rx_.search(line)) is not None:
        if line in [
            ";; ;; https://github.com/emacs-evil/evil/blob/6fde982d731e2cc4e5f6bded6f8955ab2daee3b7/evil-commands.el#L229",
            ";; https://github.com/emacs-evil/evil/blob/6fde982d731e2cc4e5f6bded6f8955ab2daee3b7/evil-commands.el#L224",
            "  ;; https://github.com/michalrus/dotfiles/blob/c4421e361400c4184ea90a021254766372a1f301/.emacs.d/init.d/040-terminal.el.symlink#L26-L48",
            ";; https://gist.github.com/ram535/a2153fb86f33ecec587d593c1c5e1623",
            "    # https://answers.microsoft.com/en-us/windows/forum/all/how-to-disable-search-the-web-completley-in/ea22410a-3031-487f-b5de-5a0113d656c5",
            "    # https://answers.microsoft.com/en-us/windows/forum/all/windows-11-right-click-explorer-menu-show-more-as/ba8dafe4-306a-403b-af0d-10a6d1ca0a9a",
            r'        r"Software\Classes\CLSID\{86ca1aa0-34aa-4e8b-a509-50c905bae2a2}\InprocServer32",',
        ]:
            return line
        print(line)
        line = rx_.sub(lambda match: "*" * len(match.group()), line)
        print(line)
    return line


def process_file(path: PurePosixPath, content_: bytes) -> bytes:
    if path.name in [
        "Cargo.lock",
        "elpaca-lock.el",
        "flake.lock",
        "flake.nix",
        "package.json",
        "poetry.lock",
        "yarn.lock",
    ]:
        return content_

    content = content_.decode()

    new_content = "\n".join(process_line(line) for line in content.split("\n"))

    return new_content.encode()


def process_tree(tree: Tree, current_path: PurePosixPath) -> Oid:
    tree_builder = repo.TreeBuilder()
    for entry in tree:
        assert entry.name is not None
        entry_path = current_path / entry.name
        if isinstance(entry, Blob):
            content = entry.data
            new_content = process_file(entry_path, content)
            GIT_OBJ_BLOB = pygit2.GIT_OBJECT_BLOB  # type: ignore
            new_blob_id = repo.write(GIT_OBJ_BLOB, new_content)
            tree_builder.insert(entry.name, new_blob_id, entry.filemode)
        elif isinstance(entry, Tree):
            new_subtree_id = process_tree(entry, entry_path)
            tree_builder.insert(entry.name, new_subtree_id, entry.filemode)
        else:
            tree_builder.insert(entry.name, entry.id, entry.filemode)
    return tree_builder.write()


def process_one(cur: Commit):
    # x.commit_time_offset
    print(f"processing: {cur.short_id} {cur.message}")

    tree = cur.tree
    new_tree = process_tree(tree, PurePosixPath())
    new_commit_id = repo.create_commit(
        None,
        cur.author,
        cur.committer,
        cur.message,
        new_tree,
        [processed[x] for x in cur.parent_ids],
        # [x for x in cur.parent_ids],
    )
    processed[cur.id] = new_commit_id
    print(new_commit_id)


for x in commits:
    process_one(x)

# process_one(head)
