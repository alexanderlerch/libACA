
#include <iostream>
#include <ctime>

#include "ACAConfig.h"

using std::cout;
using std::endl;

// local function declarations
void    showClInfo();

/////////////////////////////////////////////////////////////////////////////////
// main function
int main(int argc, char* argv[])
{

    // all done
    return 0;

}


void     showClInfo()
{
    cout << "ACA v" << ACA_VERSION_MAJOR << "." << ACA_VERSION_MINOR << "." << ACA_VERSION_PATCH << ": Demo Executable for Pitch (F0) Extraction" << endl;
    cout << "(c) 2022 by Alexander Lerch" << endl;
    cout << "Synopsis: ComputePitch inputwavfile Noveltyname [outputtxtfile] [blocksize] [hopsize]" << endl;
    cout << endl;

    return;
}
