{
    description = "williballenthin home-manager flake";

    # references:
    #  - https://discourse.nixos.org/t/home-manager-22-11-homemanagerconfiguration-pkgs-parameter/23948
    #  - https://github.com/nix-community/home-manager/issues/3075#issuecomment-1593969080
    #  - https://blog.nobbz.dev/2022-12-12-getting-inputs-to-modules-in-a-flake/
    #  - https://discourse.nixos.org/t/flakes-impure-error-installing-package-from-local-file-system/22185/9
    #  - https://discourse.nixos.org/t/improving-a-flake-nix-config-that-configures-home-manager/23389/2

    inputs = {
        # self.submodules = true;
        # 
        # nixpkgs.url = "github:nixos/nixpkgs/23.05";
        # have to track nixos-unstable since this is what home-manager dev's against
        nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
        home-manager = {
            url = "github:nix-community/home-manager";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        flake-utils.url = "github:numtide/flake-utils";
        isd.url = "github:isd-project/isd";
        cc-statusline.url = "github:williballenthin/aiwilli?dir=claude/wb/statuslines/contextusage";
        tw.url = "github:williballenthin/aiwilli?dir=tw";
        crit.url = "github:tomasz-tomczyk/crit";
        hunk.url = "github:modem-dev/hunk";
    };

  # self refers to the outputs, as they're being build, so you can reference `self.pkgs`, for example.
  outputs = { self, nixpkgs, home-manager, flake-utils, isd, cc-statusline, tw, crit, hunk, ... } @ inputs:
    let
      # via: https://github.com/nix-community/home-manager/issues/3075#issuecomment-1593969080
      mkHomeConfig = system: localModules: home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          inherit system;

          # note: I dont think this is working.
          config = {
            allowUnfree = true;
            allowUnfreePredicate = _: true;
          };
        };

        modules = [
          # Flake forwarding module.
          #
          # Expose flake inputs to our inline module below
          # https://discourse.nixos.org/t/flakes-impure-error-installing-package-from-local-file-system/22185/9
          ({...}: {
              _module.args = {
                  # inherit foo;
                  #
                  # then use below in pkgs block, like:
                  #
                  #     foo.packages.${pkgs.system}.foo
              };
          })

          ({...}: {
              _module.args = {
                inherit tw;
                inherit cc-statusline;
                inherit crit;
                inherit hunk;
              };
          })

          # Here's an inline module, receiving the normal set,
          # as well as the extra flake inputs provided via
          # _module.args above.
          ({pkgs, config, lib, ...}: {
              # home.username = "user";
              # home.homeDirectory = "/home/user";
              #
              # when we're in a flake, need to use --impure to access env vars
              home.username = (builtins.getEnv "USER");
              home.homeDirectory = (builtins.getEnv "HOME");

              home.stateVersion = "23.11";
              programs.home-manager.enable = true;

              home.packages = [
                  # expect underlying system to provide:
                  #   - ssh
                  #   - git
                  # which is reasonable, since these dotfiles come from a github repo.

                  #############################################
                  # shell and PS1
                  pkgs.fish
                  pkgs.starship

                  #############################################
                  # basic utilities
                  pkgs.ent
                  pkgs.git-lfs
                  pkgs.gnupg
                  pkgs.htop
                  pkgs.less
                  pkgs.rlwrap
                  pkgs.unzip
                  pkgs.watch
                  pkgs.zstd

                  #############################################
                  # extended utilities
                  pkgs.bat
                  pkgs.broot
                  pkgs.btop
                  pkgs.dua
                  pkgs.eza
                  pkgs.fd
                  pkgs.fzf
                  pkgs.gh
                  pkgs.gron
                  pkgs.hexyl
                  pkgs.jless
                  pkgs.jujutsu
                  pkgs.jjui
                  pkgs.jq
                  pkgs.lazygit
                  pkgs.mosh
                  pkgs.mprocs
                  pkgs.ranger
                  pkgs.rich-cli
                  pkgs.ripgrep
                  pkgs.television
                  pkgs.tig
                  pkgs.timewarrior
                  pkgs.tmux
                  # pkgs.visidata
                  pkgs.zoxide
                  pkgs.zellij
                  pkgs.herdr

                  #############################################
                  # PyPI distributed tools
                  (pkgs.python3.withPackages(ps: [
                    # temporarily disabled
                    ps.llm
                    ps.llm-gemini
                    # ps.ty  # install from uv tool for now
                    # `uv pip freeze` should be empty when this environment is rebuilt
                  ]))

                  #############################################
                  # development

                  #--------------------------------------------
                  # editors
                  pkgs.helix

                  #--------------------------------------------
                  # python
                  #
                  # uv for managing virtualenvs,
                  # not for installing global packages
                  pkgs.uv

                  #--------------------------------------------
                  # rust
                  #
                  # let rust manage itself,
                  # since we'll want to use vs code tools, etc.
                  #
                  #   rustup install stable
                  #   rustup install nightly
                  #   rustup component add rust-analyzer
                  #   rustup component add rustc-codegen-cranelift-preview --toolchain nightly
                  pkgs.rustup

                  #--------------------------------------------
                  # js
                  #
                  # so we can get claude code, which updates itself often
                  pkgs.nodejs

                  
                  #--------------------------------------------
                  # agentic tooling
                  #
                  # executable name: claude-contextusage-statusline
                  cc-statusline.packages.${pkgs.stdenv.hostPlatform.system}.default
                  tw.packages.${pkgs.stdenv.hostPlatform.system}.default
                  crit.packages.${pkgs.stdenv.hostPlatform.system}.default
                  hunk.packages.${pkgs.stdenv.hostPlatform.system}.default
              ];

              programs.direnv.enable = true;
              programs.direnv.nix-direnv.enable = true;

              home.file.".tmux.conf".source = ./tmux/.tmux.conf;
              home.file.".config/starship.toml".source = ./.config/starship.toml;
              home.file.".config/helix/config.toml".source = ./.config/helix/config.toml;
              home.file.".config/helix/languages.toml".source = ./.config/helix/languages.toml;
              home.file.".config/fish/config.fish".source = ./.config/fish/config.fish;
              home.file.".config/fish/functions/fzf.fish".source = ./.config/fish/functions/fzf.fish;
              home.file.".config/fish/functions/fish_ssh_agent.fish".source = ./.config/fish/functions/fish_ssh_agent.fish;
              home.file.".config/fish/functions/tv_smart_autocomplete.fish".source = ./.config/fish/functions/tv_smart_autocomplete.fish;
              home.file.".config/atuin/config.toml".source = ./.config/atuin/config.toml;
              home.file.".config/bat/config".source = ./.config/bat/config;
              home.file.".config/wezterm/wezterm.lua".source = ./.config/wezterm/wezterm.lua;
              home.file.".config/zellij/config.kdl".source = ./.config/zellij/config.kdl;
              home.file.".config/ghostty/config".source = ./.config/ghostty/config;
              home.file.".config/television/config.toml".source = ./.config/television/config.toml;
              home.file.".npmrc".source = ./.npmrc;
              home.activation.agentConfigs = lib.hm.dag.entryAfter ["writeBoundary"] ''
                run mkdir -p "$HOME/.claude"
                run ln -sf "$HOME/.dotfiles/.claude/CLAUDE.md" "$HOME/.claude/CLAUDE.md"

                run mkdir -p "$HOME/.pi/agent"
                run ln -sf "$HOME/.dotfiles/.pi/AGENTS.md" "$HOME/.pi/agent/AGENTS.md"
                run mkdir -p "$HOME/.pi/agent/skills/ida-domain-api"
                run ln -sf "$HOME/.dotfiles/.pi/agent/skills/ida-domain-api/SKILL.md" "$HOME/.pi/agent/skills/ida-domain-api/SKILL.md"
                run mkdir -p "$HOME/.pi/agent/skills/python"
                run ln -sf "$HOME/.dotfiles/.pi/agent/skills/python/SKILL.md" "$HOME/.pi/agent/skills/python/SKILL.md"

                run mkdir -p "$HOME/.idapro"
                run ln -sf "$HOME/.dotfiles/.idapro/idapythonrc.py" "$HOME/.idapro/idapythonrc.py";

              '';
              home.activation.miscConfigs = lib.hm.dag.entryAfter ["writeBoundary"] ''
                run mkdir -p "$HOME/.config/git"
                run ln -sf "$HOME/.dotfiles/.config/git/config" "$HOME/.config/git/config"

                run mkdir -p "$HOME/.config/zed"
                run ln -sf "$HOME/.dotfiles/.config/zed/settings.json" "$HOME/.config/zed/settings.json";
                run ln -sf "$HOME/.dotfiles/.config/zed/keymap.json" "$HOME/.config/zed/keymap.json";

                run mkdir -p "$HOME/.config/hunk"
                run ln -sf "$HOME/.dotfiles/.config/hunk/config.toml" "$HOME/.config/hunk/config.toml";

                run mkdir -p "$HOME/.config/herdr"
                run ln -sf "$HOME/.dotfiles/.config/herdr/config.toml" "$HOME/.config/herdr/config.toml";

                if [ "$(uname)" = "Darwin" ]; then
                  run mkdir -p "$HOME/Library/Application Support/lazygit"
                  run ln -sf "$HOME/.dotfiles/.config/lazygit/config.yml" "$HOME/Library/Application Support/lazygit/config.yml"
                else
                  run mkdir -p "$HOME/.config/lazygit"
                  run ln -sf "$HOME/.dotfiles/.config/lazygit/config.yml" "$HOME/.config/lazygit/config.yml"
                fi

                run ln -sf "$HOME/.dotfiles/.config/git/.gitignore" "$HOME/.config/git/.gitignore";
                run ln -sf "$HOME/.dotfiles/.config/jj/config.toml" "$HOME/.config/jj/config.toml";
              '';
          })
        ] ++ localModules;
      };
      macosLaunchAgents = {pkgs, config, lib, ...}: let
        plistSources = {
          "com.user.sync-pi-sessions" = ./machine/macos/Library/LaunchAgents/com.user.sync-pi-sessions.plist;
          "com.user.sync-claude-sessions" = ./machine/macos/Library/LaunchAgents/com.user.sync-claude-sessions.plist;
        };
        plistPackage = pkgs.runCommandLocal "launch-agent-plists" {} (
          ''mkdir -p $out
          '' +
          lib.concatStringsSep "\n" (lib.mapAttrsToList (name: src:
            ''cp ${src} $out/${name}.plist''
          ) plistSources)
        );
      in {
        home.activation.launchAgents = lib.hm.dag.entryAfter ["writeBoundary"] ''
          AGENTS=(
            com.user.sync-pi-sessions
            com.user.sync-claude-sessions
          )
          for agent in "''${AGENTS[@]}"; do
            plist="$HOME/Library/LaunchAgents/$agent.plist"
            if /bin/launchctl list "$agent" &>/dev/null; then
              run /bin/launchctl unload -w "$plist"
            fi
            run rm -f "$plist"
            run cp "${plistPackage}/$agent.plist" "$plist"
            run chmod 644 "$plist"
            run /bin/launchctl load -w "$plist"
          done
        '';
      };

    in {
      apps.aarch64-darwin.default = {
        type = "app";
        program = "${home-manager.packages.aarch64-darwin.default}/bin/home-manager";
      };
      apps.x86_64-linux.default = {
        type = "app";
        program = "${home-manager.packages.x86_64-linux.default}/bin/home-manager";
      };

      homeConfigurations."user@m1" = mkHomeConfig "aarch64-darwin" [
        macosLaunchAgents
        ({pkgs, ...}: {
            home.packages = [
              pkgs.atuin
            ];
              home.file.".config/git/config.local".source = .config/git/config.local.personal;
        })
      ];
      homeConfigurations."user@m4" = mkHomeConfig "aarch64-darwin" [
        macosLaunchAgents
        ({pkgs, ...}: {
            home.packages = [
              pkgs.atuin
            ];
              home.file.".config/git/config.local".source = .config/git/config.local.personal;
        })
      ];
      homeConfigurations."user@sb2" = mkHomeConfig "x86_64-linux" [
        ({pkgs, ...}: {
            home.packages = [
              pkgs.atuin
            ];
              home.file.".config/git/config.local".source = .config/git/config.local.personal;
        })
      ];
      homeConfigurations."user@g4" = mkHomeConfig "x86_64-linux" [
        # Flake forwarding module.
        #
        # Expose flake inputs to our inline module below
        # https://discourse.nixos.org/t/flakes-impure-error-installing-package-from-local-file-system/22185/9
        ({...}: {
            _module.args = {
                inherit isd;
            };
        })

        ({pkgs, ...}: {
            home.packages = [
              pkgs.atuin
              # systemd tui, tracked by flake during active dev
              isd.packages.${pkgs.stdenv.hostPlatform.system}.isd
            ];
            home.file.".config/git/config.local".source = .config/git/config.local.personal;
        })

        ({pkgs, lib, ...}: {
            # systemctl --user daemon-reload
            # systemctl --user start  ...
            # systemctl --user status ...
            home.file.".config/containers/systemd/navidrome.container".source = ./machine/g4/.config/containers/systemd/navidrome.container;
            # access Syncthing via SSH port forward:
            #   ssh -L 8099:localhost:8384 user@g4
            #   http://localhost:8099/
            home.file.".config/containers/systemd/syncthing.container".source = ./machine/g4/.config/containers/systemd/syncthing.container;
            home.file.".config/containers/systemd/metube.container".source = ./machine/g4/.config/containers/systemd/metube.container;
            home.file.".config/containers/systemd/vaultwarden.container".source = ./machine/g4/.config/containers/systemd/vaultwarden.container;
            home.file.".config/containers/systemd/uptime-kuma.container".source = ./machine/g4/.config/containers/systemd/uptime-kuma.container;
            home.file.".config/containers/systemd/jellyfin.container".source = ./machine/g4/.config/containers/systemd/jellyfin.container;
            home.file.".config/containers/systemd/pinchflat.container".source = ./machine/g4/.config/containers/systemd/pinchflat.container;
            home.file.".config/containers/systemd/archivebox.container".source = ./machine/g4/.config/containers/systemd/archivebox.container;

            # on first run, need to login to Tailscale.
            # review the service log output (journalctl) for the login link.
            # subsequently, the machine keys are stored in a volume.
            home.file.".config/containers/systemd/tsnsrv-navidrome.container".source = ./machine/g4/.config/containers/systemd/tsnsrv-navidrome.container;
            home.file.".config/containers/systemd/tsnsrv-metube.container".source = ./machine/g4/.config/containers/systemd/tsnsrv-metube.container;
            home.file.".config/containers/systemd/tsnsrv-vaultwarden.container".source = ./machine/g4/.config/containers/systemd/tsnsrv-vaultwarden.container;
            home.file.".config/containers/systemd/tsnsrv-uptime-kuma.container".source = ./machine/g4/.config/containers/systemd/tsnsrv-uptime-kuma.container;
            home.file.".config/containers/systemd/tsnsrv-jellyfin.container".source = ./machine/g4/.config/containers/systemd/tsnsrv-jellyfin.container;
            home.file.".config/containers/systemd/tsnsrv-pinchflat.container".source = ./machine/g4/.config/containers/systemd/tsnsrv-pinchflat.container;
            home.file.".config/containers/systemd/tsnsrv-archivebox.container".source = ./machine/g4/.config/containers/systemd/tsnsrv-archivebox.container;

            # on first run, need to login to Google.
            # use a standalone podman container to initialize the secrets using its documentation.
            # subsequently, the machine keys are stored in a volume.
            home.file.".config/containers/systemd/gphotos-sync.container".source = ./machine/g4/.config/containers/systemd/gphotos-sync.container;
            home.file.".config/systemd/user/gphotos-sync.timer".source = ./machine/g4/.config/systemd/user/gphotos-sync.timer;

            # prior to first run, need to create ~/.config/restic/sync.env
            # which should look like:
            #
            #     RESTIC_PASSWORD=your_repo_password
            #     RESTIC_REPOSITORY=rest:http://192.168.1.200/your-repo/
            home.file.".config/systemd/user/restic-sync.service".source = ./machine/g4/.config/systemd/user/restic-sync.service;
            home.file.".config/systemd/user/restic-sync.timer".source = ./machine/g4/.config/systemd/user/restic-sync.timer;

            # requires prior `ob login` and `ob sync-setup` to initialize credentials
            # also requires `npm install -g obsidian-headless` using global npm right now
            home.file.".config/systemd/user/obsidian-sync.service".source = ./machine/g4/.config/systemd/user/obsidian-sync.service;

            # requires ~/code/aiwilli checkout and ~/code/aiwilli/secrets.env
            # see secrets.env.example in that repo for format
            home.file.".config/systemd/user/weave.service".source = ./machine/g4/.config/systemd/user/weave.service;

            # sync agent session transcripts (Claude, Pi) into Syncthing
            home.file.".config/systemd/user/agent-session-sync.service".source = ./machine/g4/.config/systemd/user/agent-session-sync.service;
            home.file.".config/systemd/user/agent-session-sync.timer".source = ./machine/g4/.config/systemd/user/agent-session-sync.timer;
            home.activation.systemdUnits = lib.hm.dag.entryAfter ["writeBoundary"] ''
              run /usr/bin/systemctl --user daemon-reload

              # timers managed by home-manager (enable + restart)
              TIMERS=(
                gphotos-sync.timer
                restic-sync.timer
                agent-session-sync.timer
              )
              for timer in "''${TIMERS[@]}"; do
                run echo "enble,restart $timer"
                run /usr/bin/systemctl --user enable "$timer"
                run /usr/bin/systemctl --user restart "$timer"
              done

              # services managed by home-manager (enable + restart)
              HM_SERVICES=(
                obsidian-sync.service
                weave.service
              )
              for svc in "''${HM_SERVICES[@]}"; do
                run echo "enble,restart $svc"
                run /usr/bin/systemctl --user enable "$svc"
                run /usr/bin/systemctl --user restart "$svc"
              done

              # quadlet-generated services (restart only; enable is not supported)
              QUADLET_SERVICES=(
                navidrome.service
                syncthing.service
                metube.service
                vaultwarden.service
                uptime-kuma.service
                jellyfin.service
                pinchflat.service
                archivebox.service
                tsnsrv-navidrome.service
                tsnsrv-metube.service
                tsnsrv-vaultwarden.service
                tsnsrv-uptime-kuma.service
                tsnsrv-jellyfin.service
                tsnsrv-pinchflat.service
                tsnsrv-archivebox.service
              )
              for svc in "''${QUADLET_SERVICES[@]}"; do
                run echo "enble,restart $svc"
                run /usr/bin/systemctl --user restart "$svc"
              done
            '';
        })
      ];
      homeConfigurations."user@w" = mkHomeConfig "x86_64-linux" [
        ({pkgs, ...}: {
            home.packages = [
              pkgs.google-cloud-sdk
            ];
            home.file.".config/git/config.local".source = .config/git/config.local.personal;
        })
      ];
      homeConfigurations."user@hr" = mkHomeConfig "aarch64-darwin" [
        macosLaunchAgents
        ({pkgs, ...}: {
            home.packages = [
              pkgs.atuin
              pkgs.glab
              pkgs.jankyborders
              pkgs.podman
            ];
            home.file.".config/git/config.local".source = .config/git/config.local.hexrays;

            home.file.".config/borders/bordersrs".source = .config/borders/bordersrc;
        })
      ];
    };
}
