{lib
, python3
, fetchFromGitHub
}:

python3.pkgs.buildPythonPackage rec {
  pname = "llm-gemini";
  # bump to 0.14.1 or beyond once pkgs.llm is 0.23 (has schema)
  version = "0.12";
  pyproject = true;
  dontCheckRuntimeDeps = true;

  src = fetchFromGitHub {
    owner = "simonw";
    repo = "llm-gemini";
    rev = version;
    # hash = "sha256-ejZIClYRKyL68LFMjshLYLuaP7qK9mHqoKtfXwBN01U=";  # 0.14.1
    # hash = "sha256-JlzJFpHqeLMlzmU2GhIXolhDfjx/5CqeXXi4FUspPhs=";  # 0.13
    hash = "sha256-U9JFGwHeWKQ37gFWo3t0jnZfjDHEvgC8Yc3V3icIEq0=";  # 0.12
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
