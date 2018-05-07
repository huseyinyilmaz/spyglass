#============================ build stage ============================
FROM fpco/stack-build:lts-11.7 as builder
RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc
#============================= run stage =============================
FROM ubuntu:16.04
RUN mkdir -p /opt/spyglass
ARG BINARY_PATH
WORKDIR /opt/spyglass
RUN apt-get update && apt-get install -y \
  ca-certificates \
  libgmp-dev
COPY --from=builder /opt/build/.stack-work/install/x86_64-linux/lts-9.9/8.0.2/bin .
# COPY static /opt/spyglass/static
# COPY config /opt/spyglass/config
CMD ["/opt/spyglass/spyglass"]
