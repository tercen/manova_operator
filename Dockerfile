FROM tercen/runtime-r44:4.4.3-11
COPY main.R /operator/main.R
WORKDIR /operator
ENTRYPOINT ["R", "--no-save", "--no-restore", "--no-environ", "--slave", "-f", "main.R", "--args"]
