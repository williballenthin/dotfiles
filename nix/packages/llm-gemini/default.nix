{lib
, python3
, fetchFromGitHub
}:

python3.pkgs.buildPythonPackage rec {
  pname = "llm-gemini";
  version = "0.14.1";
  pyproject = true;
  dontCheckRuntimeDeps = true;

  src = fetchFromGitHub {
    owner = "simonw";
    repo = "llm-gemini";
    rev = version;
    hash = "sha256-ejZIClYRKyL68LFMjshLYLuaP7qK9mHqoKtfXwBN01U=";
  };

  nativeBuildInputs = [
    python3.pkgs.setuptools
    python3.pkgs.wheel
  ];

  propagatedBuildInputs = with python3.pkgs; [
    httpx
    ijson
  ];

  meta = with lib; {
    description = "Plugin for LLM adding support for Google's Gemini models";
    license = licenses.asl20;
    mainProgram = "llm-gemini";
  };
}
