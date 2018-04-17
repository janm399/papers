#include <iostream>
#include <easylogging++.h>
#include "envelopes.pb.h"
#include "messages.pb.h"

#include "oom_attack.hpp"

INITIALIZE_EASYLOGGINGPP

using namespace com::acme::envelopes;

int main(int argc, char** argv) {
    User user;
    user.set_date("");
    user.set_id("asdad");
    user.set_name("Jan");

    Envelope envelope;
    envelope.set_correlation_id("");
    envelope.set_token("43");
    envelope.mutable_payload()->PackFrom(user);

    LOG(INFO) << envelope.SerializeAsString();

    Envelope envelope2;
    envelope2.ParseFromString(envelope.SerializeAsString());
    User user2;
    envelope2.payload().UnpackTo(&user2);

    LOG(INFO) << user.ShortDebugString();

    oom_attack();

    return 0;
}
