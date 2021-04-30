# The `unsafe-html-cube-devcontainer` is built in the
# initializeCommand for the devcontainer from base.dockerfile
# which is located in the .devcontainer directory
FROM unsafe-html-cube-devcontainer:15.7.0

ENV ELM_VERSION=0.19.0
ENV ELM_TEST_VERSION 0.19.0
ENV ELM_FORMAT_VERSION 0.8.4
ENV ELM_VERIFY_EXAMPLES_VERSION 5.0.0
ENV ELM_DOC_PREVIEW_VERSION 5.0.5

USER $USERNAME

WORKDIR /home/$USERNAME

ENV HISTFILE /home/$USERNAME/bash_history/bash_history.txt

# Install the development specific ones
RUN yarn global add \
        elm@$ELM_VERSION \
        elm-test@$ELM_TEST_VERSION \
        elm-format@$ELM_FORMAT_VERSION \
        elm-verify-examples@$ELM_VERIFY_EXAMPLES_VERSION \
        elm-doc-preview@$ELM_DOC_PREVIEW_VERSION \
    # Create the elm cache directory where we can mount a volume. If we don't create it like this
    # it is auto created by docker on volume creation but with root as owner which makes it unusable.
    && mkdir .elm \
    # Similar story here with the bash history we store in a volume
    && mkdir -p $(dirname $HISTFILE)

ENV PATH "$PATH:/home/$USERNAME/.yarn/bin"

RUN echo 'PATH="$PATH:/home/$USERNAME/.yarn/bin"' >> .bashrc
