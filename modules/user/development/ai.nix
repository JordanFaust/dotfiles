{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with lib.my; let
  cfg = config.modules.development.ai;
  inherit (config.modules) minimal;
in {
  options.modules.development.ai = mkOption {
    description = ''
      Configurations for AI development tools.
    '';
    type = with lib.types;
      nullOr (submoduleWith {
        modules = [
          {
            options = {
              enable = mkEnableOption "ai";
            };
          }
        ];
      });
    default = null;
  };

  config = lib.mkIf (!minimal && cfg != null && cfg.enable) {
    home = {
      packages = with pkgs; [
        # Opencode
        opencode
        # Ollama - local LLM runner (Go implementation)
        ollama
        # llm - call various language models from the terminal
        llm
        # files-to-prompt - concatenate files/dirs into LLM-ready context
        files-to-prompt
        # tokencount - count tokens in a file or directory by vendor/model
        (pkgs.writers.writePython3Bin "tokencount"
          {
            libraries = with pkgs.python3Packages; [
              tiktoken
              anthropic
            ];
          }
          ''
            import argparse
            import os
            import sys

            import tiktoken


            IGNORE_DIRS = {
                "evals", ".git", "node_modules", "__pycache__",
                ".cache",
            }

            IGNORE_EXTENSIONS = {
                ".png", ".jpg", ".jpeg", ".gif", ".ico", ".svg",
                ".pdf", ".zip", ".tar", ".gz", ".bin", ".lock",
            }


            def read_path(path):
                if os.path.isfile(path):
                    return {path: open(path).read()}
                if os.path.isdir(path):
                    files = {}
                    for root, dirs, names in os.walk(path):
                        dirs[:] = [
                            d for d in dirs
                            if d not in IGNORE_DIRS
                            and not d.startswith(".")
                        ]
                        for name in names:
                            if name.startswith("."):
                                continue
                            ext = os.path.splitext(name)[1]
                            if ext in IGNORE_EXTENSIONS:
                                continue
                            fpath = os.path.join(root, name)
                            try:
                                files[fpath] = open(fpath).read()
                            except (UnicodeDecodeError, IOError):
                                pass
                    return files
                print("Path not found: {}".format(path))
                sys.exit(1)


            def count_openai(texts, model):
                try:
                    enc = tiktoken.encoding_for_model(model)
                except KeyError:
                    enc = tiktoken.get_encoding("cl100k_base")
                return {
                    p: len(enc.encode(t)) for p, t in texts.items()
                }


            ANTHROPIC_SHORTHANDS = {
                "sonnet": (
                    "ANTHROPIC_DEFAULT_SONNET_MODEL",
                    "anthropic.claude-sonnet-4-6",
                ),
                "opus": (
                    "ANTHROPIC_DEFAULT_OPUS_MODEL",
                    "anthropic.claude-opus-4-6-v1",
                ),
                "haiku": (
                    "ANTHROPIC_DEFAULT_HAIKU_MODEL",
                    "anthropic.claude-haiku-4-5-20251001-v1:0",
                ),
            }


            def resolve_anthropic_model(model):
                entry = ANTHROPIC_SHORTHANDS.get(model)
                if entry:
                    env_var, default = entry
                    return os.environ.get(env_var, default)
                return model


            def count_anthropic(texts, model):
                import anthropic
                auth_token = os.environ.get("ANTHROPIC_AUTH_TOKEN")
                if not auth_token:
                    print(
                        "Error: ANTHROPIC_AUTH_TOKEN is not set."
                    )
                    sys.exit(1)
                base_url = os.environ.get("ANTHROPIC_BASE_URL")
                client = anthropic.Anthropic(
                    auth_token=auth_token,
                    **({"base_url": base_url} if base_url else {}),
                )
                results = {}
                for path, text in texts.items():
                    resp = client.messages.count_tokens(
                        model=model,
                        messages=[
                            {"role": "user", "content": text}
                        ],
                    )
                    results[path] = resp.input_tokens
                return results


            def print_results(results, vendor, model):
                if len(results) == 1:
                    count = next(iter(results.values()))
                    print("{} tokens ({}/{})".format(
                        count, vendor, model
                    ))
                    return
                total = 0
                for path, count in sorted(results.items()):
                    print("{:>8}  {}".format(count, path))
                    total += count
                print("-" * 40)
                print("{:>8}  total ({}/{})".format(
                    total, vendor, model
                ))


            def main():
                parser = argparse.ArgumentParser(
                    description=(
                        "Count tokens in a file or directory"
                    ),
                    formatter_class=(
                        argparse.RawDescriptionHelpFormatter
                    ),
                    epilog=(
                        "Vendors / models:\n"
                        "  openai   gpt-4, gpt-4-turbo,"
                        " gpt-3.5-turbo, gpt-4o (default)\n"
                        "  anthropic  sonnet (default),"
                        " opus, haiku, or full model string\n"
                        "             resolves shorthands via"
                        " ANTHROPIC_DEFAULT_*_MODEL env vars\n\n"
                        "Examples:\n"
                        "  tokencount file.txt\n"
                        "  tokencount file.txt"
                        " --vendor openai --model gpt-4o\n"
                        "  tokencount ./skills/"
                        " --vendor anthropic\n"
                        "  tokencount ./skills/"
                        " --vendor anthropic --model opus\n"
                    ),
                )
                parser.add_argument(
                    "path",
                    help="File or directory to tokenize",
                )
                parser.add_argument(
                    "--vendor",
                    choices=["openai", "anthropic"],
                    default="openai",
                    help="Model vendor (default: openai)",
                )
                parser.add_argument(
                    "--model",
                    help="Model name (vendor-specific default"
                    " used if omitted)",
                )
                args = parser.parse_args()

                if args.vendor == "openai":
                    model = args.model or "gpt-4o"
                else:
                    model = resolve_anthropic_model(
                        args.model or "sonnet"
                    )

                texts = read_path(args.path)

                if args.vendor == "openai":
                    results = count_openai(texts, model)
                else:
                    results = count_anthropic(texts, model)

                print_results(results, args.vendor, model)


            main()
          '')
        # skillbundle - bundle a skill's files and show token counts (or print content)
        (pkgs.writers.writePython3Bin "skillbundle"
          {
            libraries = with pkgs.python3Packages; [
              tiktoken
              anthropic
            ];
          }
          ''
            import argparse
            import os
            import sys

            import tiktoken


            SKILLS_DIR = os.path.expanduser("~/.claude/skills")

            EXCLUDE_DIRS = {
                "eval-viewer", "evals", "assets",
                "node_modules", "__pycache__", ".git",
            }

            EXCLUDE_FILES = {"LICENSE.txt", "LICENSE"}

            EXCLUDE_EXTENSIONS = {
                ".png", ".jpg", ".jpeg", ".gif", ".ico", ".svg",
                ".pdf", ".zip", ".tar", ".gz", ".bin", ".lock", ".pyc",
            }


            def find_skill(name):
                path = os.path.join(SKILLS_DIR, name)
                if os.path.isdir(path):
                    return path
                try:
                    entries = os.listdir(SKILLS_DIR)
                except OSError:
                    print("Skills directory not found: {}".format(SKILLS_DIR))
                    sys.exit(1)
                matches = [e for e in entries if e.startswith(name)]
                if len(matches) == 1:
                    return os.path.join(SKILLS_DIR, matches[0])
                if len(matches) > 1:
                    print("Ambiguous skill '{}': {}".format(
                        name, ", ".join(sorted(matches))
                    ))
                    sys.exit(1)
                print("Skill not found: {}".format(name))
                sys.exit(1)


            def collect_files(skill_path):
                result = []
                seen = set()

                def add(path):
                    if path in seen:
                        return
                    seen.add(path)
                    try:
                        content = open(path).read()
                    except (UnicodeDecodeError, IOError):
                        return
                    rel = os.path.relpath(path, skill_path)
                    result.append((rel, content))

                skill_md = os.path.join(skill_path, "SKILL.md")
                if os.path.isfile(skill_md):
                    add(skill_md)
                for root, dirs, names in os.walk(skill_path):
                    dirs[:] = sorted(
                        d for d in dirs
                        if d not in EXCLUDE_DIRS and not d.startswith(".")
                    )
                    for name in sorted(names):
                        if name in EXCLUDE_FILES or name.startswith("."):
                            continue
                        if os.path.splitext(name)[1] in EXCLUDE_EXTENSIONS:
                            continue
                        add(os.path.join(root, name))
                return result


            def count_openai(texts, model):
                try:
                    enc = tiktoken.encoding_for_model(model)
                except KeyError:
                    enc = tiktoken.get_encoding("cl100k_base")
                return {p: len(enc.encode(t)) for p, t in texts.items()}


            ANTHROPIC_SHORTHANDS = {
                "sonnet": (
                    "ANTHROPIC_DEFAULT_SONNET_MODEL",
                    "anthropic.claude-sonnet-4-6",
                ),
                "opus": (
                    "ANTHROPIC_DEFAULT_OPUS_MODEL",
                    "anthropic.claude-opus-4-6-v1",
                ),
                "haiku": (
                    "ANTHROPIC_DEFAULT_HAIKU_MODEL",
                    "anthropic.claude-haiku-4-5-20251001-v1:0",
                ),
            }


            def resolve_anthropic_model(model):
                entry = ANTHROPIC_SHORTHANDS.get(model)
                if entry:
                    env_var, default = entry
                    return os.environ.get(env_var, default)
                return model


            def count_anthropic(texts, model):
                import anthropic
                auth_token = os.environ.get("ANTHROPIC_AUTH_TOKEN")
                if not auth_token:
                    print("Error: ANTHROPIC_AUTH_TOKEN is not set.")
                    sys.exit(1)
                base_url = os.environ.get("ANTHROPIC_BASE_URL")
                client = anthropic.Anthropic(
                    auth_token=auth_token,
                    **({"base_url": base_url} if base_url else {}),
                )
                results = {}
                for path, text in texts.items():
                    resp = client.messages.count_tokens(
                        model=model,
                        messages=[{"role": "user", "content": text}],
                    )
                    results[path] = resp.input_tokens
                return results


            def main():
                parser = argparse.ArgumentParser(
                    description=(
                        "Bundle a Claude Code skill's files and show token counts"
                    ),
                    formatter_class=argparse.RawDescriptionHelpFormatter,
                    epilog=(
                        "Examples:\n"
                        "  skillbundle skill-creator\n"
                        "  skillbundle skill-cr          (prefix match)\n"
                        "  skillbundle skill-creator --print | pbcopy\n"
                        "  skillbundle skill-creator --vendor anthropic\n"
                    ),
                )
                parser.add_argument("skill", help="Skill name or prefix")
                parser.add_argument(
                    "--print", dest="print_content", action="store_true",
                    help="Print concatenated file content to stdout",
                )
                parser.add_argument(
                    "--vendor", choices=["openai", "anthropic"], default="openai",
                )
                parser.add_argument("--model", help="Model name")
                args = parser.parse_args()

                skill_path = find_skill(args.skill)
                skill_name = os.path.basename(skill_path)
                files = collect_files(skill_path)

                if not files:
                    print("No files found in skill: {}".format(skill_name))
                    sys.exit(1)

                if args.print_content:
                    for rel, content in files:
                        print('<file path="{}">'.format(rel))
                        print(content)
                        print("</file>")
                        print()
                    return

                texts = dict(files)

                if args.vendor == "openai":
                    model = args.model or "gpt-4o"
                    results = count_openai(texts, model)
                else:
                    model = resolve_anthropic_model(args.model or "sonnet")
                    results = count_anthropic(texts, model)

                total = 0
                print("{}:".format(skill_name))
                for rel, count in sorted(
                    results.items(),
                    key=lambda x: (x[0] != "SKILL.md", x[0]),
                ):
                    print("  {:>8}  {}".format(count, rel))
                    total += count
                print("  " + "-" * 38)
                print("  {:>8}  total ({}/{})".format(total, args.vendor, model))


            main()
          '')
        # skillsearch - search Claude Code skills by name or description
        (pkgs.writers.writePython3Bin "skillsearch"
          {libraries = [];}
          ''
            import argparse
            import os
            import re
            import sys


            SKILLS_DIR = os.path.expanduser("~/.claude/skills")


            def parse_frontmatter(text):
                m = re.match(r'^---\n(.*?)\n---', text, re.DOTALL)
                if not m:
                    return None, None
                fm = m.group(1)
                name_m = re.search(r'^name:\s*(.+)$', fm, re.MULTILINE)
                desc_m = re.search(r'^description:\s*(.+)$', fm, re.MULTILINE)
                return (
                    name_m.group(1).strip() if name_m else None,
                    desc_m.group(1).strip() if desc_m else None,
                )


            def load_skills():
                try:
                    entries = sorted(os.listdir(SKILLS_DIR))
                except OSError:
                    print("Skills directory not found: {}".format(SKILLS_DIR))
                    sys.exit(1)
                skills = []
                for entry in entries:
                    skill_md = os.path.join(SKILLS_DIR, entry, "SKILL.md")
                    if not os.path.isfile(skill_md):
                        continue
                    try:
                        text = open(skill_md).read()
                    except IOError:
                        continue
                    name, desc = parse_frontmatter(text)
                    skills.append({
                        "name": name or entry,
                        "description": desc or "",
                    })
                return skills


            def main():
                parser = argparse.ArgumentParser(
                    description="Search Claude Code skills by name or description",
                    formatter_class=argparse.RawDescriptionHelpFormatter,
                    epilog=(
                        "Examples:\n"
                        "  skillsearch\n"
                        "  skillsearch debugging\n"
                        "  skillsearch skill\n"
                    ),
                )
                parser.add_argument(
                    "query", nargs="?",
                    help="Substring to match in name or description",
                )
                args = parser.parse_args()

                skills = load_skills()

                if args.query:
                    q = args.query.lower()
                    skills = [
                        s for s in skills
                        if q in s["name"].lower() or q in s["description"].lower()
                    ]

                if not skills:
                    msg = "No skills found"
                    if args.query:
                        msg += " matching '{}'".format(args.query)
                    print(msg)
                    sys.exit(0)

                width = max(len(s["name"]) for s in skills)
                for s in skills:
                    if s["description"]:
                        print("{:<{w}}  {}".format(
                            s["name"], s["description"], w=width
                        ))
                    else:
                        print(s["name"])


            main()
          '')
      ];
    };
  };
}
