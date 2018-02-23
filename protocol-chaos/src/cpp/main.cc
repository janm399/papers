#include <iostream>
#include <easylogging++.h>

#include "oom_attack.hpp"

INITIALIZE_EASYLOGGINGPP

using namespace com::acme::protocolchaos;

int main(int argc, char** argv) {
    oom_attack();

    return 0;
}
