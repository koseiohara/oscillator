class namelist:

    @classmethod
    def read(self, filename):

        output_dict = {}

        file = open(filename, 'r')
        for line in file:


            # Get the name of the namelist
            if (line[0] == '&' and line[:4].lower() != '&end'):
                nml = line[1:].strip()
                
                # Initialize
                output_dict[nml] = {}
                continue

            # Ignore the end statement or blank lines
            if (line == '\n' or line.strip() == '' or line[0] == '/' or line[0] == '&'):
                continue

            line = line.strip()
            equal = line.index('=')
            # get the name of the parameter
            param_name = line[:equal].strip()
            # get the parameter (param_data is still a string)
            param_data = line[equal+1:].strip()

            # Convert param_data to the correct data type (int, float, int-list, float-list, bool, or string are available)
            output_dict[nml][param_name] = self.__str_converter(param_data)
            
        file.close()

        return output_dict


    def __str_converter(input_str):
        try:
            return int(input_str)
        except:
            pass

        try:
            return float(input_str)
        except:
            pass

        if (input_str.lower() == '.true.'):
            return True
        elif (input_str.lower() == '.false.'):
            return False
        # If the first character is " or ', ONE string is output
        elif (input_str[0] == "'" or input_str[0] == '"'):
            return input_str[1:-1]
        else:
            str_copy = input_str
            # for integer array
            try:
                str_copy = str_copy.split(',')
                for i in range(len(str_copy)):
                    str_copy[i] = int(str_copy[i].strip())
                return str_copy
            except:
                pass

            str_copy = input_str
            # for float array
            try:
                str_copy = str_copy.split(',')
                for i in range(len(str_copy)):
                    str_copy[i] = float(str_copy[i].strip())
                return str_copy
            except:
                pass

            # If the type of the input parameter is unknowen, output without modification
            return input_str


