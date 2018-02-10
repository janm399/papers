#include <iostream>
#include <easylogging++.h>

INITIALIZE_EASYLOGGINGPP

int main(int argc, char** argv) {
    LOG(INFO) << "main()";
    return 0;
}
